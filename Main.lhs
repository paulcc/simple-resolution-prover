> module Main where

> import Text.PrettyPrint(render)
> import System.Environment(getArgs)

> import TermStuff(PrettyPrintable(..), ppi, mk_var)

> import Parser
> import NormalForm
> import Search


> main :: IO ()
> main
>  = do
>       -- mapM_ (\s -> p_p s >> putStrLn "") examples
>       as <- getArgs
>       case as of
>         []      -> tf "eg_two"            -- zero args, do eg_two
>         [n]     -> tf_n (read n) "eg_two" -- one arg, do it n times
>         (n:f:_) -> tf_n (read n) f        -- choice of runs and file

> p_p s
>  = do
>       putStrLn $ "Try: " ++ s ++ "\n" 
>       case parse s of
>         Nothing     -> putStrLn $ "Failed on: " ++ s
>         Just (hs,c) -> putStrLn $ unlines $ map (render.pp) (hs ++ [c])


> examples
>  = map (\s -> "Conclude " ++ s ++";")
>    [ "a -> b"
>    , "forall X . p(X) -> q(X) | r(X)"
>    , "forall X . p(X) -> exists Y. q(X,Y)"
>    , "p(fred) & forall X . p(X) -> exists Y. q(X,Y)"
>    , "p(fred) -> q(bill,son(jack)) -> q(son(X), son(jack))"
>    ]

%----------------------------------------------------------------------------

Belongs elsewhere.

> mk_neg_wff hs c 
>  = foldr (:&) (Not c) hs        -- ok for right-assoc tree? 

> parse_m :: String -> IO ([WFF],WFF)
> parse_m s
>  = do
>       case parse s of
>         Nothing -> fail $ "Failed on: " ++ s
>         Just r  -> return r

> pf n 
>  = do
>       ls <- readFile n
>       (hs,c) <- parse_m ls
>       putStr $ unlines $ map show (hs ++ [c])



---
Running proof from test file, doing it a number of times.

> tf = tf_n 1

> tf_n :: Int -> String -> IO ()
> tf_n n f 
>  = do
>       ls <- readFile f
>       (hs,c) <- parse_m ls
>       sequence_ [ do_proof i (hs,c) | i <- [1 .. n - 1] ]
>       (nfcs, cs) <- do_proof n (hs,c)
>       putStrLn $ nfcs ++ "\n"
>       putStrLn $ cs
>       writeFile (f ++ ".o") cs

> do_proof i (hs,c)
>  = do
>       let nf = to_cnf $ mk_neg_wff hs c
>       let nfcs = unlines $ out nf
>       case dfs_search nf of
>         Left m   -> do
>                       putStrLn "proof failed"
>                       return $ (nfcs, "** FAIL **\n" ++ m)
>         Right cs -> do
>                       putStrLn $ "proof done: " ++ show i
>                       return $ (nfcs, done cs)
>    where
>       out :: PrettyPrintable x => [x] -> [String]
>       out = map (render.pp)
>       done cs = unlines $ "** SUCCEED ** " : out cs


%---------------------------------------

> xf f 
>  = do
>       ls <- readFile f
>       (hs,c) <- parse_m ls
>       let nf = to_cnf $ mk_neg_wff hs c
>	putStrLn $ "done cnf"
>       mapM_ ppi nf
>	putStrLn $ "printed cnf"
>       process $ dfs_trace nf 

> out :: PrettyPrintable x => [x] -> [String]
> out = map (render.pp)

> process (Right s : rs)
>  = putStr "\n\n\nDone:\n" >> mapM_ ppi (get_path s)
> process (Left s : rs)  
>  = do 
>		case get_path s of
>		  [] -> putStrLn "nothing yet"
>		  cs -> ppi $ last_derived s
>		putStrLn ""
>		process rs

