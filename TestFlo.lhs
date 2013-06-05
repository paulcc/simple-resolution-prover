> module TestFlo where

> import Terms 

> flo 
>  = (ForAll (mk_var "X") (Lit (Pred "fc" [Var (mk_var "X")]) :> ForAll (mk_var "Y") (Lit (Pred "p" [Var (mk_var "Y")]) :> Lit (Pred "s" [Var (mk_var "X"),Var (mk_var "Y")]))))
>  :& (Exists (mk_var "X") (Lit (Pred "p" [Var (mk_var "X")])))
>  :& (ForAll (mk_var "X") (ForAll (mk_var "Y") (Lit (Pred "c" [Var (mk_var "X")]) :& Lit (Pred "p" [Var (mk_var "Y")]) :> Lit (Pred "s" [Var (mk_var "X"),Var (mk_var "Y")]) :> Lit (Pred "b" [Var (mk_var "X")]))))
>  :& (ForAll (mk_var "X") (Lit (Pred "fc" [Var (mk_var "X")]) :> Lit (Pred "c" [Var (mk_var "X")])))
>  :& (Lit (Pred "fc" [Fn "Florence" []]))
>  :& Not (Lit (Pred "b" [Fn "Florence" []]))

