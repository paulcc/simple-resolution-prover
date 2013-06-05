> module Unify where

> import TermStuff

%----------------------------------------------------------------------------
Overloading the unify function
 - we can unify at several levels of representation, eg literals, terms
 - the SUPERCLASS says, anything Unifiable should be Substitutible
    - this is to ensure we can apply substitutions during unification
 - the unify function can produce any substitution (ie, it only uses 
   the class interface)
 - unification doesn't always succeed, so we use Maybe to represent possible
   failure.
 - notice that the monadicity of Maybe is used to write the unification 
   algorithm in an imperative style - which is justified, since steps need
   to be done in a certain order, and we should stop at the first failure.
   

> class Substitutible t => Unifiable t where
>   unify :: Substitution s => t -> t -> Maybe s




%---------------------------------------
Literals 
 - literals unify if the predicate name is the same, and the lists of 
   arguments unify
 - notice that the impl depends on unifying lists (see next)

> instance Unifiable Literal where
>   unify (Pred l_n l_ts) (Pred r_n r_ts)
>    | l_n /= r_n = fail "predicates don't match"
>    | otherwise  = unify l_ts r_ts



%---------------------------------------
Lists of things
 - if we can unify two things, we can unify lists of them
 - empty lists unify - clearly they match
 - unequal length lists DON'T match, so return failure.

 - fail :: Monad m => String -> m a
    - this is another part of the monad class, used for reporting error msgs
    - for Maybe, fail x = Nothing, so the `error message' is ignored.

 - notice use of do-notation in the main case
    - if the heads of the lists unify, get a substitution
    - then apply the subst. to the remaining items in the lists, and 
      try to unify the tails of the lists
    - if this succeeds, merge the substitutions, and return the result
    - notice the arg. order - the first subst is extended with the second;
      in my (unenforceable) intention, the second subst may solve some 
      variables in the earlier subst.
    - if either unification fails, the whole computation fails

> instance Unifiable a => Unifiable [a] where
>   unify [] [] = return emptySubstitution
>   unify [] _  = fail "unequal lists in unification"
>   unify _  [] = fail "unequal lists in unification"
>   unify (l:ls) (r:rs) = do
>                           s1 <- unify l r 
>                           s2 <- unify (apply s1 ls) (apply s1 rs) 
>                           return (s1 `extend_with` s2)


%---------------------------------------
Term unification
 - the most interesting case.
 - identical variables match, to give the empty substitution 
 - for unequal vars, we want to replace one by the other
    - the order is arbitrarily chosen - but it doesn't matter which
    - no cycle will appear: if there was a problem before, then an
      earlier substitution would have eliminated this instance; and 
      there will be no problem in the future because this substitution
      will eliminate the discrepancy from here onwards.
 - for var/non-var, we do an occurs-check
    - this is to avoid cycles appearing in the substitution (which we
      check for in the ListSubstitution code anyway)
 - for fn/fn cases, the names must match and the lists of arguments 
   must unify.

> instance Unifiable Term where
>   unify (Var l_v) r@ (Var r_v) 
>    | l_v == r_v = return $ emptySubstitution
>    | otherwise  = return $ l_v +-> r        -- choose arbitrarily

>   unify (Var l_v)  r@(Fn _ _)

>    | l_v `elem` vars_in r = fail "occurs-check"
>    | otherwise            = return $ l_v +-> r

>   unify l@(Fn _ _) (Var r_v)
>    | r_v `elem` vars_in l = fail "occurs-check"
>    | otherwise            = return $ r_v +-> l

>   unify (Fn l_n l_ts) (Fn r_n r_ts)
>    | l_n /= r_n = fail "functions don't match"
>    | otherwise  = unify l_ts r_ts



%----------------------------------------------------------------------------

EXERCISE: a question to think about - what about unification on WFFs? can 
		  it be defined?

