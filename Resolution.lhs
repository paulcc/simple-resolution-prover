> module Resolution (unify_clauses) where

> import Data.List((\\), intersect, nub)
> import Data.Maybe(catMaybes)

> import TermStuff 
> import Unify
> import ListSubstitution

%----------------------------------------------------------------------------
Forgot to say this earlier...

> instance Substitutible Clause where
>   apply ss (Clause ns ps) = Clause (apply ss ns) (apply ss ps)



%----------------------------------------------------------------------------
Unify clauses
 - First standardise apart (to avoid conflict), then apply BINARY resolution 
   on the basis of a pair of complementary literals
 - Note the use of field names to denote 'assignment' - a new piece of syntax

> unify_clauses :: Clause -> Clause -> Maybe (ListSubstitution, Clause)
> unify_clauses lc rc
>  = do
>		let lc' = lc `standardise_apart_wrt` rc
>		unify_clauses_ lc' rc


> unify_clauses_ lc rc
>  | null l_neg__r_pos && null l_pos__r_neg
>  = Nothing

>  | not $ null l_neg__r_pos
>  = case head l_neg__r_pos of
>       ((l,r),s) -> Just 
>                  $ ((,) s)
>                  $ apply_factoring
>                  $ apply s 
>                  $ Clause{ neg_lits = (neg_lits lc \\ [l] ) ++ neg_lits rc
>                          , pos_lits = pos_lits lc ++ (pos_lits rc \\ [r])}
>  | otherwise
>  = case head l_pos__r_neg of
>       ((l,r),s) -> Just 
>                  $ ((,) s)
>                  $ apply_factoring
>                  $ apply s 
>                  $ Clause{ neg_lits=neg_lits lc ++ (neg_lits rc \\ [r])
>                          , pos_lits=(pos_lits lc \\ [l]) ++ pos_lits rc}
>    where
>       l_neg__r_pos = find_complementary_pairs (neg_lits lc) (pos_lits rc)
>       l_pos__r_neg = find_complementary_pairs (pos_lits lc) (neg_lits rc)


---
Factoring rule
 - this is needed when using the simple binary form of resolution
        (sometimes you can get duplicated literals which can't be removed 
         in any other way.)
 - it performs unification where possible between literals in same list.
 - it is harder to explain than write the code for! 

 - Example:   p(X) \/ p(Y) \/ q(X) \/ p(fred)
              -------------------------------
                    q(fred) \/ p(fred) 

 - Thus, the p(X), p(Y) are redundant in search terms - 
     they carry NO NEW INFO

 - "factor_by_lit" sweeps through the list, unifying where possible with a 
   target literal. When unification succeeds, the substitution is propagated
   through all literals and the current literal is dropped (because it is 
   now subsumed by the target). Non-unifying literals are kept for later. 

   Thus the result is: a literal which is the combination of all unifications
   with the target literal, plus a list of non-unifying literals. Each sweep 
   can remove several literals from the original list.

 - "factor_lit_list" iterates the "factor_by_lit" process.
   Whilst there are literals to process, take the first as target and apply
   "factor_by_lit" to the rest. On return, pick up the target result, and 
   try again on the literals not unifying with the target. Repeat until
   there's no more literals left.

> apply_factoring :: Clause -> Clause
> apply_factoring c
>  = Clause{ neg_lits = factor_lit_list (neg_lits c)
>          , pos_lits = factor_lit_list (pos_lits c) }

> factor_lit_list :: [Literal] -> [Literal]
> factor_lit_list [] = []
> factor_lit_list (x:xs) 
>  = case factor_by_lit (x,[]) xs of
>      (x, ls) -> x : factor_lit_list ls

> factor_by_lit :: (Literal, [Literal]) -> [Literal] -> (Literal, [Literal])
> factor_by_lit (x,ns) [] = (x,ns)
> factor_by_lit (x,ns) (y:ys)
>  = case (unify x y :: Maybe ListSubstitution) of
>      Nothing -> factor_by_lit (x,         ns ++ [y])  ys
>      Just s  -> factor_by_lit (apply s x, apply s ns) (apply s ys)



---
`find_complementary_pairs'
  - does what it says, returning all possible pairs of literals 
  - in general, we should find (and unify) complementary LISTS of literals
  - note that we return the pair plus their MGU.

> find_complementary_pairs
>  :: Substitution s => [Literal] -> [Literal] -> [((Literal,Literal),s)]
> find_complementary_pairs as bs
>  = catMaybes [ do { s <- unify a b; return ((a,b),s) } 
>              | a <- as, b <- bs ]


%----------------------------------------------------------------------------
Standardisation-apart of a clause with respect to another clause.
 - returns a substitution that changes vars in the FIRST clause that occur 
   in the second (target) clause to vars that appear in neither clause.
 - we need to do this before unifying clauses, to avoid clashes of vars
 - remember: the vars aren't the important thing in resolution - what is
   important is how constants/functions are propagated through... 

> standardise_apart_wrt :: Clause -> Clause -> Clause
> clause `standardise_apart_wrt` target
>  = apply (subst :: ListSubstitution) clause
>    where
>       used_vars     = vars_in clause
>       vars_to_avoid = vars_in target
>       overlap  = used_vars `intersect` vars_to_avoid
>       new_vars = pick_new_vars overlap
>       subst = foldr extend_with emptySubstitution
>                        [ o +-> Var n | (o,n) <- zip overlap new_vars ]

