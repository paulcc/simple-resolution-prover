> module Terms (
>                VarName, mk_var, pick_new_vars     -- as an ADT.
> 
>              , Term(..), Literal(..), WFF(..)
>              , Clause(..), is_empty_clause
>
>              , ContainsVars(..)
>              ) where

Term structure, and basic operations.

> import Data.List (union)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Basic term structure.
 - note the use of LAYERS of types to provide some constraints

---
Well-formed formulae.
 - a tree type for representing expressions in FOL
 - am using operator symbols as constructors, and can set their P & A.
 - the precedence mirrors the boolean operators.

> infixl 3 :&
> infixl 2 :|
> infixr 1 :>

> data WFF 
>  = Lit Literal         -- predicates/propositions
>  | FALSE
>  | TRUE
>  | Not WFF             -- negation
>  | WFF :& WFF          -- conjunction
>  | WFF :| WFF          -- disjunction
>  | WFF :> WFF          -- implication
>  | ForAll VarName WFF  -- Universal Qfn
>  | Exists VarName WFF  -- Existential Qfn
>    deriving Show


Note: 
  Normally, I wouldn't leave "deriving Show" because we're about to define 
    a better version. But I leave it here, for your convenience.
  Eq on Literals and Terms is needed later, where exact comparison is needed.


%---------------------------------------
Literals and Terms
 - literals are the things found at leaves of logical formulae
 - they are a predicate name applied to zero or more terms
 - terms are a function name applied to zero or more terms
 - a constant is a 0-ary function
 - note that terms can contain terms, ie the type is recursive.

> data Literal
>  = Pred String [Term]
>    deriving (Eq,Show)

> data Term 
>  = Var VarName
>  | Fn String [Term]
>    deriving (Eq,Show)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Variables
  - variables are `protected' via an ADT-style interface. 
  - we can only build new vars, Show & Eq them, and generate fresh variables.
  - allows us to silently change the underlying representation, if needed.

> data VarName 
>  = VarName String
>    deriving Eq

> instance Show VarName where
>   show (VarName v) = v

---
Construct a VarName

> mk_var :: String -> VarName
> mk_var = VarName


---
Create VarNames which are guaranteed not to be in the set given.
 - technique: create an infinite list, and filter out the used ones.
 - callers can then take as many as they need

> pick_new_vars :: [VarName] -> [VarName]
> pick_new_vars all_vars
>  = [ v | v <- [ mk_var $ "X" ++ show n | n <- [0..] ]
>        , v `notElem` all_vars
>    ] 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Finding the FREE variables used in a value
 - A frequently used operation
 - we use a class to OVERLOAD the function
 - we'd like to use the operation on several types


> class ContainsVars t where
>   vars_in :: t -> [VarName]


---
If we know how to get vars from a `t', then we can get the vars from 
a list of them by taking the UNION of the vars in each element of the list.

This gives a consistent treatment for lists of var-containing values, and
saves some inconvenience later (we don't have to remember how to treat lists)

> instance ContainsVars a => ContainsVars [a] where
>   vars_in xs = foldl union [] [ vars_in x | x <- xs ]
>   -- uses union to merge lists
>   -- uses foldl because it is cheaper (union filters on its second arg.)

---
Now, instances for the standard parts of terms.
These are straightforward, except:
 - from subtrees, take the (set) union of the vars from each side
 - under quantifiers, the var is bound, so remove this from the free
   variables in the quantified term (`body').

> instance ContainsVars Term where
>   vars_in (Var v)     = [v]
>   vars_in (Fn _ ts)   = vars_in ts

> instance ContainsVars Literal where
>   vars_in (Pred _ ts) = vars_in ts

> instance ContainsVars WFF where
>   vars_in (Lit a) = vars_in a
>   vars_in TRUE = []
>   vars_in FALSE = []
>   vars_in (Not w) = vars_in w
>   vars_in (l :& r) = vars_in l `union` vars_in r
>   vars_in (l :| r) = vars_in l `union` vars_in r
>   vars_in (l :> r) = vars_in l `union` vars_in r
>   vars_in (ForAll v w) = filter (/= v) $ vars_in w 
>   vars_in (Exists v w) = filter (/= v) $ vars_in w




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A datatype for clauses
 - Used to represent WFFs in conjunctive normal form (CNF)
 - A clause is the DISJUNCTION of a set of positive literals and a set of 
   negative literals
 - A list of clauses is intended as a CONJUNCTION of the clauses.
 - Literals are just Predicate expressions. 
 - NB Show and Eq will be handled elsewhere.

 - This representation also called KOWALSKI form, and can be viewed as 
   the conjunction of the negatives implying the disjunction of the 
   positives, ie:   
          "n1 & ... & nM -> p1 | ... | pN"

> data Clause 
>  = Clause { neg_lits :: [Literal]
>           , pos_lits :: [Literal]
>           }

> instance ContainsVars Clause where
>   vars_in c = vars_in $ neg_lits c ++ pos_lits c

---
Testing for a contradiction
 - `false' is a contradiction

> is_empty_clause :: Clause -> Bool
> is_empty_clause c = null (neg_lits c) && null (pos_lits c)

