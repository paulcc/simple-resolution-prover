> module NormalForm (to_cnf) where

Converting a Term to its CNF.

> import Control.Monad(liftM2)

> import StateMonad
> import TermStuff

> import ListSubstitution
>   -- import a specific implementation, to be used in rectification and 
>   -- skolemisation only.

> -- rather than use a type cast, I use a local function to force use of
> -- this implementation.
> ls_apply :: Substitutible t => ListSubstitution -> t -> t
> ls_apply = apply 

%----------------------------------------------------------------------------
Converting to CNF is done in four stages.
 - negation-normal form -- all Not's pushed to leaves.
 - rectification        -- all vars unique
 - skolemisation        -- quantifier removal
 - flattening to CNF    -- squash and-or tree to Conj of Disj of Literals.

> to_cnf :: WFF -> [Clause]
> to_cnf = cnf . skolemise . rectify . nnf . add_implicit_binds


---
add_implicit_binds
 - add forall-binders for all free variables in the term.

> add_implicit_binds :: WFF -> WFF
> add_implicit_binds t0
>  = foldr (\v t -> ForAll v t) t0   -- add a forall-binder for each
>  $ vars_in t0                      -- all free variables


%----------------------------------------------------------------------------
Negation-normal form
 - 'Not' only appears applied to atoms.
 - and '->' is expanded to (~a \/ b)
 - you can view this as pushing the 'Not's down to the leaves
 -

> nnf :: WFF -> WFF 

> nnf      (a :> b)  = nnf      (Not a :| b)
> nnf (Not (a :> b)) = nnf (Not (Not a :| b))

> nnf (Not (a :& b)) = nnf (Not a) :| nnf (Not b)
> nnf (Not (a :| b)) = nnf (Not a) :& nnf (Not b)
> nnf (Not (Not a))  = nnf a
> nnf (Not (ForAll v w)) = Exists v (nnf (Not w))
> nnf (Not (Exists v w)) = ForAll v (nnf (Not w))
> nnf na@(Not (Lit _)) = na
> nnf (Not t) = error $ "Missed Not: " ++ show t

> nnf a@(Lit _) = a
> nnf (a :& b) = nnf a :& nnf b
> nnf (a :| b) = nnf a :| nnf b
> nnf (Exists v w) = Exists v (nnf w)
> nnf (ForAll v w) = ForAll v (nnf w)


%----------------------------------------------------------------------------
Rectification
 - this is a renaming of variables, such that:
    - no FV (free var) has the name of any BV (bound var)
    - quantifiers each use different vars.
 - it simplifies the later stages.
 -
 - First notice we need some kind of state - to see which variables have been
   used, plus a way to generate fresh variable names.
 - I use a trick: generate an infinite list of variables that is guaranteed
   not to contain the free variables in the term (via pick_new_vars)
 - Definition using state monad is now straightforward.

> rectify :: WFF -> WFF 
> rectify t
>  = fst $ runST vars_to_use $ rectify_ t
>    where 
>       free_vars = vars_in t
>       vars_to_use = pick_new_vars free_vars 

> replacement_var :: ST [VarName] VarName
> replacement_var 
>  = do
>       (v:vs) <- getST
>       setST vs
>       return v

---
`rectify_'
  - does the actual work - simple structural recursion, with the messy 
    stuff hidden away in a monad.
  - interesting cases are the quantifiers - just grab a new variable,
    substitute it for the old in the body, then rectify the body.
  - else, it is just a (monadic) traversal of the tree, rebuilding by
    combining the rectified subtrees.

> rectify_ :: WFF -> ST [VarName] WFF

> rectify_ (Exists v w)
>  = do
>       new_var <- replacement_var 
>       w' <- rectify_ (ls_apply (v +-> Var new_var) w)
>       return (Exists new_var w')

> rectify_ (ForAll v w)
>  = do
>       new_var <- replacement_var
>       w' <- rectify_ (ls_apply (v +-> Var new_var) w)
>       return (ForAll new_var w')

> rectify_ (a :& b)
>  = do
>       r_a <- rectify_ a 
>       r_b <- rectify_ b
>       return (r_a :& r_b)

> rectify_ (a :| b)
>  = do
>       r_a <- rectify_ a 
>       r_b <- rectify_ b
>       return (r_a :| r_b)

> rectify_ a@(Lit _)        = return a
> rectify_ na@(Not (Lit _)) = return na



%----------------------------------------------------------------------------
Skolemisation
 - another state-based algorithm - we need a list of fresh names, AND 
   to keep a list of universally-bound variables in scope.
 - notice that the states are different - fresh names are global, list of
   bound vars is local (to the subtree) - but I treat them as global, for 
   convenience.

> skolemise :: WFF -> WFF
> skolemise
>  = fst . runST init_state . sk
>    where
>       init_state = ([], skolem_names)
>       skolem_names = [ "_f" ++ show n | n <- [0..] ]

    NB assuming skolem fn names can't be introduced by user - so they ARE
    INTERNAL

---
`sk' does the hard work.

> type State = ([VarName], [String])

> sk (ForAll v w)
>  = do
>       chgST (\(vs,ns) -> (v:vs,ns))        -- push
>       w' <- sk w 
>       chgST (\(_:vs,ns) -> (vs,ns))        -- pop
>       return w'

> sk (Exists v w) 
>  = do
>       (vs, n:ns) <- getST
>       setST (vs, ns)
>       let fn = Fn n (map Var vs)
>       sk (ls_apply (v +-> fn) w)

> sk t@(Lit _)       = return t
> sk t@(Not (Lit _)) = return t
> sk (a :& b) = liftM2 (:&) (sk a) (sk b)
> sk (a :| b) = liftM2 (:|) (sk a) (sk b)
>   -- last 2 cases say: do the subtrees, and combine the results with
>   -- the given constructor. (defn of liftM2 below)


%----------------------------------------------------------------------------
CNF
 - wff is now an and-or tree, with Lit or Not-Lit at the leaves 
 - this stage means flattening it to conjunction of disjunctions of literals
 - disjunctions are represented as clauses, with positive and negative parts

> cnf :: WFF -> [Clause]

> cnf t@(Lit a)       = [Clause []  [a]]
> cnf t@(Not (Lit a)) = [Clause [a] []]

> cnf (a :& b) = cnf a ++ cnf b

> cnf (a :| b)
>  = [ Clause (n_a ++ n_b) (p_a ++ p_b) 
>    | Clause n_a p_a <- cnf a
>    , Clause n_b p_b <- cnf b
>    ]



%----------------------------------------------------------------------------
Test cases

> p n = Lit $ Pred n [] 


> t1 = Pred "P" [Fn "Q" [Var $mk_var "a", Var $mk_var "b"]]

> t2 = Exists (mk_var "a")
>      $ ForAll (mk_var "b")
>      $ Lit t1 :> Not (Lit t1)

> t3 = t2 :& Not t2



---
Definition of liftM2
 - used to `lift' a 2-ary function to monadic level
 - ie, given a (m a) and a (m b), do the monads to get a,b out
 - then return result of applying function to a,b

<> liftM2   :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
<> liftM2 f m_a m_b 
<>  = do 
<>      a <- m_a
<>      b <- m_b
<>      return (f a b)


