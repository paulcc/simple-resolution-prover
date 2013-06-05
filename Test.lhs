> module Test where

> import TermStuff
> import NormalForm
> import Unify

<> t1 = Pred "b" [Fn "a" [Var $VarName 0, Var $VarName 1]]
<> t2 = ForAll (VarName 0) (Atom t1)

> s1 :: Substitutible t => t -> t
> s1 = subst (Var (VarName "a") // VarName "b")

%---------------------------------------

> t1 = Pred "P" [Fn "Q" [Var $VarName "a", Var $VarName "b"]]

> t2 = Exists (VarName "a") 
>      $ ForAll (VarName "b") 
>      $ Atom t1 :> Not (Atom t1)

> t3 = t2 :& Not t2

> equ a b = (a :> b) :& (b :> a)
> p n = Atom $ Pred n []

> t4 = (p "P" `equ` p "Q") `equ` p "R"
> c4 = to_cnf t4

%-------------------
Contrapos.
 - negated, for theorem.

> t5 = (p "P" :> p "Q") :> Not (p "Q") :> Not (p "P")
> c5 = to_cnf (Not t5)

%-------------------

> t6 = ((Not (p "P") :| p "Q") :& Not (p "R")) :| p "P"
> c6 = to_cnf t6

%---------------------------------------

> sc :: [Clause] -> IO ()
> sc = mapM_ p_c


%---------------------------------------

-- > uc a b = sc [ (\(Just x) -> x) $ unify_clauses a b ]
