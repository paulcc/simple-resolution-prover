> module Printing (PrettyPrintable(..), render, ppi, pps) where

Pretty-printing for terms and clauses

> import Text.PrettyPrint
> import Terms


%---------------------------------------
Precis of main pretty-printing operators 
  -- see hugs lib (Pretty.lhs) for more info.
  -- this is a Domain-specific Language (DSL) for describing doc layout
  -- remember the Html library from 2H FP? Similar idea.

Doc
render :: Doc -> String
  -- an abstract type of documents, and a function for laying out the doc.

text :: String -> Doc
  -- embed a string in a doc.

(<>), (<+>), ($$) :: Doc -> Doc -> Doc
  -- combine docs - horizontal join, ditto with space, vertical join

cat, hcat, sep, hsep :: [Doc] -> Doc
  -- combine lists of docs, either horizontally or vertically, or by whatever
     fits

parens, brackets :: Doc -> Doc
  -- wrap a doc in parentheses etc.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
We want to use prettyprinting on several types, hence using a class 
is good here.

> class PrettyPrintable a where
>   pp :: a -> Doc

---
Some useful functions 
 - prettyprint to string, or to screen (via IO)

> pps :: PrettyPrintable a => a -> String        
> pps = render . pp 
> ppi :: PrettyPrintable a => a -> IO ()
> ppi = putStrLn . pps


%---------------------------------------

> instance PrettyPrintable VarName where
>   pp v = text $ show v

> instance PrettyPrintable Term where
>   pp (Var v)   = pp v
>   pp (Fn n ts) = pp_application n ts

> instance PrettyPrintable Literal where
>   pp (Pred n ts) = pp_application n ts

---
Handles applications of functions or predicates to lists of terms.
 - there is a special case for 0-ary applications (just show the name)

> pp_application :: PrettyPrintable a => String -> [a] -> Doc
> pp_application n [] = text n
> pp_application n (t:ts)
>  =  text n
>  <> char '(' 
>  <> pp t <> hcat [char ',' <> pp t | t <- ts ]
>  <> char ')'


%---------------------------------------

> instance PrettyPrintable WFF where
>   pp TRUE         = text "true"
>   pp FALSE        = text "false"
>   pp (Not t)      = text "~" <> char '(' <> pp t <> char ')' 
>   pp (Lit a)      = pp a
>   pp (l :& r)     = parens $ sep [pp l, text "&", pp r]
>   pp (l :| r)     = parens $ sep [pp l, text "|", pp r] 
>   pp (l :> r)     = parens $ sep [pp l, text "->", pp r] 
>   pp (ForAll v w) = parens $ sep [ hsep [ text "ForAll", pp v, text "."]
>                                  , pp w]
>   pp (Exists v w) = parens $ sep [ hsep [ text "Exists", pp v, text "."]
>                                  , pp w]


%---------------------------------------
Note the nesting in the display of clauses: 
 - on the left, the word "clause"
 - on the right, two vertically-stacked lists of literals 
 - check the output with a test case/ see florence output

> instance PrettyPrintable Clause where
>   pp (Clause ns ps)
>    = text "Clause:" <+> vcat [ text "--" <+> vcat (map pp ns)
>                              , text "++" <+> vcat (map pp ps) ]

%----------------------------------------------------------------------------
Optional Exercise: put in Precedence and Associativity by hand
%----------------------------------------------------------------------------
