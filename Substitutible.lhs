> module Substitutible where

Gives an interface for (implementations of) Substitutions, and says how
to apply these to WFFs etc.

> import Terms


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Substitutions.
  - A substitution is a (functional) mapping from variables to terms, such 
    that no domain variable appears in any of the (range) terms

  - applying a substitution is a standard manipulation on a WFF/Term

  - terminology - the vars being mapped are the domain variables
                - the results of mapping one of the domain variables is
                  its range term. 
  - The interface is enshrined in the class below (cf a Java interface)
     - the basic op. is looking up a variable, result Nothing or a Term
     - a new substitution is created with op. (+->) or emptySubstitution
     - substitutions are extended by `extend_with', where the information
       contained in the second subst is added to the first. 
       
       The second subst can change variables contained in the range terms 
       of the first, representing more knowledge gained - by solving more
       variables. Think of it _updating_ the first subst. 
       Eg (V1 +-> f(V2)) `extend_with` (V2 +-> fred)  -- is ok, 
                   -- gives effect of [(V1 +-> f(fred)), (V2 +-> fred)]

     - the interface gives us enough information to write required functions


  - Some of these operations can fail, if there's a bug in the program
     - eg, a direct circularity: (V1 +-> f(V1)) 
     - eg, indirect circularity: (V1 +-> f(V2)) `extend_with` (V2 +-> V1)
     - eg, domain overlaps: (V1 +-> f(V2)) `extend_with` (V1 +-> g(V6))
            (what should V1 map to?)
     - in the way I use substitutions, these situations should not occur
     - but such _programming_ mistakes are caught by calls to "error".
  
  - QUESTION: can we write code so it is automatically safe? 

  - Using a class is another way of making something ABSTRACT. All we know 
    is that a type may exist which provides precisely those functions/values


> class ContainsVars s => Substitution s where
>   emptySubstitution :: s
>   lookup_var  :: s -> VarName -> Maybe Term
>   extend_with :: s -> s -> s
>   (+->)       :: VarName -> Term -> s



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A class representing the ability to apply substitutions
 - applying a substitution means, for a substition whose domain variables
   are the set V, replace all FREE occurrences of vars in V with their 
   range terms.
 - bound variables can't be touched!

 - we want to use function `apply' on several levels of WFF structure
 - overloading is useful for this.
 - the signature of `apply' says: it will work with anything that implements
   the "Substitution" interface
 - this allows the specific implementations of `apply' to use anything from
   the interface

> class Substitutible t where
>     apply :: Substitution s => s -> t -> t 


QUESTION: Does the process of applying a substitution always terminate? 
          How can we be sure that it doesn't fall in to an infinite loop? 


---
A substitution distributes over a list of things
 - to apply a substitution to a list of things, use map to apply it
   to each of them.
 - this is useful - avoids need for explicit mapping over a list

> instance Substitutible a => Substitutible [a] where
>   apply ss = map (apply ss)


---
In a term
 - vars might change, depending on whether the var is in the mapping
 - else we propagate the substitution over the function terms.

> instance Substitutible Term where
>   apply ss (Fn n ts) = Fn n (apply ss ts)
>   apply ss v@(Var n) = case lookup_var ss n of
>                          Nothing -> v
>                          Just t  -> t


---
Just pass the substitution over the arguments of the predicate

> instance Substitutible Literal where
>   apply ss (Pred n ts) = Pred n (apply ss ts)


---
Most of substitution over WFFs is obvious, except for the binder-cases.
 - for the binder case, check if the bound var appears in the substitution
 - if it is, then naive substitution might produce incorrect results
 - so: replace the bound var FIRST with a fresh var, to avoid risk of capture
 - otherwise, no need to change it.

 - Note: I don't combine the substitutions, but use apply twice.
    - The problem case occurs when both arguments of extend_with map the
      same variable
    - We'd need to be very careful, to decide which side is over-ridden.
    - My version is defensive - I explicitly change the offending bound
      var before applying the original substitution. 
    - THINK: how easy is it to make a mistake here?  (too easy)

> instance Substitutible WFF where
>      apply ss (Lit a)  = Lit $ apply ss a
>      apply ss TRUE     = TRUE
>      apply ss FALSE    = FALSE
>      apply ss (Not w)  = Not $ apply ss w
>      apply ss (l :& r) = apply ss l :& apply ss r
>      apply ss (l :| r) = apply ss l :| apply ss r
>      apply ss (l :> r) = apply ss l :> apply ss r

>      apply ss (ForAll v w)
>        = case need_fresh ss v of
>           Nothing -> ForAll v  $ apply ss w
>           Just nv -> ForAll nv $ apply ss 
>                                $ apply ((v +-> Var nv) `asTypeOf` ss)
>                                $ w

>      apply ss (Exists v w)
>        = case need_fresh ss v of
>           Nothing -> Exists v  $ apply ss w
>           Just nv -> Exists nv $ apply ss 
>                                $ apply ((v +-> Var nv) `asTypeOf` ss)
>                                $ w


asTypeOf: 
  Above is an example of classes introducing ambiguity
  We know that 'ss' has the type of the substitution argument
  But, when we create the renaming substitution, there's NO INFORMATION to 
    say what instance of Substitution it should be. 
  A common trick is to use `asTypeOf' to force the type to be identical
    to a known type, here the type of the input substitution.
  asTypeOf's explicit signature forces its two arguments to have the same 
    type (technically speaking, the types should be unifiable...)
  It doesn't use its second argument - just returns the first argument
    (NB function (\x y -> x) has type (a -> b -> a), so the constrained 
        signature is necessary here!)

<> asTypeOf :: a -> a -> a
<> asTypeOf x y = x

%---------------------------------------
`need_fresh' 
  - checks whether a var is used in the substitution.
  - if so, then selects a fresh var which isn't used there
  - else returns Nothing
  -
  - I've made the type a bit more specific than necessary, to signal
    the intended use of the function (it could just use ContainsVars)

> need_fresh :: Substitution s => s -> VarName -> Maybe VarName
> need_fresh ss v
>  | v `notElem` subst_vars
>     = Nothing
>  | otherwise
>     = Just $ head $ pick_new_vars subst_vars
>  where
>       subst_vars = vars_in ss 




