> module Lexer (all_tokens, lexer, test_lexer, Token(..)) where

Hand-written Lexer - should really use a auto-generated lexer.

*** YOU DON'T NEED TO STUDY THIS MODULE. ***

> import Data.Char
> import Data.Maybe (isJust, fromJust)
> import Data.List  (nub, sortBy, isPrefixOf)

%----------------------------------------------------------------------------
Type to represent lexer return values.

> data Token
>  = TokenEOF 
>  | TokenFail String
>
>  | TokenSemi                -- ; 
>  | TokenComma                -- ,
>  | TokenDot                -- .
>  | TokenBracket Char        -- () {} [] etc
>  | TokenOp String

>  | TokenKeyword String
>  | TokenVar String        -- all upper case
>  | TokenId String            -- mixed case
>  | TokenLitInt Integer
>    deriving Show

%----------------------------------------------------------------------------

> all_tokens :: String -> Maybe [Token]
> all_tokens = error_free . do_until_end lexer

> error_free ts
>  = case [ s | (TokenFail s) <- ts ] of
>   []  -> Just ts
>   s:_ -> Nothing

> do_until_end :: (String -> (Token,String)) -> String -> [Token]
> do_until_end f a
>  | null a    = []
>  | otherwise = let (b,a') = f a in 
>                case b of 
>                  TokenEOF -> [] 
>                  b        -> b : do_until_end f a'


%----------------------------------------------------------------------------

> lexer :: String -> (Token, String)
> lexer [] = (TokenEOF, [])

> lexer ('-':'-':cs)
>  = lexer (dropWhile (/='\n') cs)        -- comments

> lexer (c:cs) 
>  | isSpace c || isControl c = lexer cs

> lexer (';':cs) = (TokenSemi,  cs)
> lexer (',':cs) = (TokenComma, cs)
> lexer ('.':cs) = (TokenDot,   cs)

> lexer (c:cs) 
>  | c `elem` brackets = (TokenBracket c, cs)
>   where
>       brackets = "(){}[]"

> lexer s@(c:cs)
>  | c `elem` symbol_chars && isJust match
>     = fromJust match
>    where
>       match = symbol_matches s

> lexer s@(c:cs)
>  | isDigit c 
>     = (TokenLitInt $ read digits, dropWhile isSpace not)
>    where
>       (digits, not) = span isDigit s

> lexer s
>  | null first
>     = (TokenFail $ "Didn't lex:" ++ s ++ error "\ncrashing\n", s)
>  | first `elem` keywords 
>     = (TokenKeyword first, after)
>  | isUpper (head first) && all (\c -> isUpper c || isDigit c) (tail first)
>     = (TokenVar first, after)
>  | otherwise 
>     = (TokenId first, after)
>    where
>       (first,rest) = span is_id_char s
>       after = dropWhile isSpace $ drop (length first) s
>       is_id_char c = isAlphaNum c || c == '_' || c == '\''


%----------------------------------------------------------------------------
Keywords

> keywords 
>  = words "forall exists true false Conclude"



%----------------------------------------------------------------------------
Symbols
 - starting point is list of possible combinations
 - want a greedy match on this.
 - fn given - not too efficient but it is simple - could use tree instead...

> symbols :: [String]
> symbols = words "== != -> & | ~"

> symbol_chars :: String
> symbol_chars = nub $ concat $ symbols

> symbol_matches :: String -> Maybe (Token,String)
> symbol_matches s 
>  = case (filter (`isPrefixOf` s) symbols) of
>       [] -> Nothing
>       ss -> Just $ 
>             chop_off $ 
>             head $
>             sortBy increasing_length ss 
>    where
>       increasing_length a b = length b `compare` length a
>       chop_off sym = let n = length sym in (TokenOp sym, drop n s)

%----------------------------------------------------------------------------

> test_lexer = putStr . unlines . map show . do_until_end lexer

> t1 = test_lexer "while \"abc\" 1 2s a{ then  {} () ++ + == != else"
