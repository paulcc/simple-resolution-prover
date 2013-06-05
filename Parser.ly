> {
> module Parser (parse, WFF(..)) where

Parser for first-order language.

*** YOU DON'T NEED TO STUDY THIS MODULE. ***

> import Terms
> import Lexer
> }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Parser config
 - says how the lexer should be used
 - then maps lexer output to parser tokens.

> %name parse_aux
> %tokentype { Token }
> %token 
>       -- key words
>       'true'          { TokenKeyword "true" }
>       'false'         { TokenKeyword "false" }
>       'forall'        { TokenKeyword "forall" }
>       'exists'        { TokenKeyword "exists" }
>       'Conclude'      { TokenKeyword "Conclude" }

>       -- identifiers/variables
>       ID              { TokenId  $$ }
>       VAR             { TokenVar $$ }

>       -- punctuation and symbols
>       ';'             { TokenSemi }
>       ','             { TokenComma }
>       '.'             { TokenDot }
>       '=='            { TokenOp "==" }
>       '!='            { TokenOp "!=" }
>       '&'             { TokenOp "&" }
>       '|'             { TokenOp "|" }
>       '->'            { TokenOp "->" }
>       '~'             { TokenOp "~"  }

>       -- bracketing
>       '('             { TokenBracket '(' }
>       ')'             { TokenBracket ')' }

-- precedence, increase as go downwards.

> %right    '->'
> %left     '|'
> %left     '&'
> %nonassoc '==' '!='
> %right    '~'

> %%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
`argument' - series of hypotheses then a conclusion.

> argument :: { ([WFF], WFF) }
>  : wffs 'Conclude' wff opt_semi      { ($1, $3) }

> opt_semi :: { () }
>  :      { () }
>  | ';'  { () }

---
`wffs' - a list of wff items, each followed by a semicolon.

> wffs :: { [WFF] }
>  :              { [] } 
>  | wff ';' wffs { $1 : $3 }


---
`wff' - as per the type structure

> wff :: { WFF }
>  : literal              { Lit $1 }
>  | 'true'               { TRUE }
>  | 'false'              { FALSE }
>  | '~' wff              { Not $2 }
>  | wff '&' wff          { $1 :& $3 }
>  | wff '|' wff          { $1 :| $3 }
>  | wff '->' wff         { $1 :> $3 }
>  | 'forall' VAR '.' wff { ForAll (mk_var $2) $4 }
>  | 'exists' VAR '.' wff { Exists (mk_var $2) $4 }
>  | '(' wff ')'          { $2 }
>  

---
`literal' 
  - a predicate name can be any (non-variable) identifier.
  - there's a special case for (infix) equality  -- might not be used

> literal :: { Literal }
>  : ID  opt_args { Pred $1 $2 }
>  | term '==' term { Pred "Q" [$1,$3] }


---
`opt_args' etc 
  - functions and predicates have zero or more arguments 
  - note how this section is recursive.

> opt_args :: { [Term] }
>  :                             { [] }
>  | '(' term more_opt_args ')'  { $2 : $3 }

> more_opt_args :: { [Term] }
>  :                             { [] }
>  | ',' term more_opt_args      { $2 : $3 }

> term :: { Term }
>  : VAR                         { Var (mk_var $1) }
>  | ID opt_args                 { Fn $1 $2 }



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Any other code

> {

> happyError ts
>  = error ("Parse error, from tokens: " ++ show (take 5 ts))


---
`parse'
  - maps a string to an argument structure (hypotheses, conclusions)

> parse :: String -> Maybe ([WFF],WFF) 
> parse s 
>  = case (all_tokens s) of
>		Nothing -> Nothing
>		Just ts -> Just $ parse_aux ts 

> }

