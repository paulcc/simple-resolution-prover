> module Search ( dfs_search, brfs_search
>               , dfs_trace, get_path, last_derived) where

> -- import Trace
> -- import IOExts(trace)
> import Data.Maybe(catMaybes)
> import Data.List((\\), sortBy)

> import TermStuff
> import Resolution
> import Gtree_search

%----------------------------------------------------------------------------
Main search routine.
 - this implements the "LINEAR INPUT" strategy, with a choice of depth-first 
   or breadth-first traversal of the search space.
 - Linear input strategy is discussed later.

> dfs_trace cs = search_trace depth_first (init_state cs)

> dfs_search :: [Clause] -> Either String [Clause]
> dfs_search = do_linear_search new_dfs

> brfs_search :: [Clause] -> Either String [Clause]
> brfs_search = do_linear_search new_brfs

> do_linear_search 
>  :: (Linear -> [Linear]) -> [Clause] -> Either String [Clause]
> do_linear_search search_fn cs 
>  = case search_fn (init_state cs) of
>         []     -> Left  $ "No solution found"
>         (gs:_) -> Right $ reverse $ derived gs





%----------------------------------------------------------------------------
LinearSearch
 - the strategy says: at each stage, one of the clauses to resolve must
   be an "input" clause (ie, one of the original clauses input)
 - this cuts down the possibilities in searching
 - BUT: it is complete ONLY for Horn clauses (see below). 
 -
 - Hence, representation - separate the input clauses from the derived 
   clauses. (This is all the state we need in searching.)

> data Linear 
>  = Linear { input   :: [Clause]
>           , derived :: [Clause]
>           }

> last_derived = head . derived
> get_path = derived

---
Showing the state 
 - just show the derived clauses, with most recent at top

> instance Show Linear where
>   show (Linear is ds) = unlines $ map pps ds


---
init_state
 - test whether state can be initialised
 - a precondition - problem must only use Horn clauses
 - then, arbitrarily chooses last clause as the starting clause (I am
   ASSUMING that this is the conclusion clause...)

> init_state :: [Clause] -> Linear
> init_state cs
>  | all is_horn_clause cs 
>     = Linear cs []
>  | otherwise 
>     = error $ "A clause was non-Horn."

---
`all' - tests predicate on a list of values

<> all p xs = and [ p x | x <- xs ]


---
`is_horn_clause'
  - contains at most one un-negated literal, ie less than two pos literals

> is_horn_clause :: Clause -> Bool
> is_horn_clause c = length (pos_lits c) < 2


---
Now, fit up the search problem in the framework from last year.
 - stop when we derive the empty clause
 - successors are generated by trying to unify the most recent derived 
   clause with ALL of the input clauses.

> instance Searchable Linear where
>   is_goal (Linear _ (d:_)) = is_empty_clause d
>   is_goal _                = False
>   successors = next_states

---
Empty case:
  - make all clauses possible start clauses
  - but reverse, so we try the last (conventionally the conclusion) first

> next_states (Linear is [])
>  = {-# SCC "next_states-init" #-}
>  ( reverse
>  $ [ Linear (take i is ++ drop (i+1) is) [is !! i]
>    | i <- [0 .. length is - 1] ]
>  ) 


> next_states (Linear is (d:ds)) 
>  = {-# SCC "next_states-step" #-}
>  ( catMaybes [ do { (_,c) <- unify_clauses d i
>                   ; let c' = -- trace ("ICL=\n" ++ pps i ++ "\n") $ 
>                              order_clause c
>                   ; return $ Linear is (c':d:ds) }
>              | i <- is ]
>  )

> order_clause c 
>  = Clause {neg_lits = sortBy prefer_Q $ neg_lits c
>           ,pos_lits = sortBy prefer_Q $ pos_lits c
>           }
>    where
>	is_Q (Pred s _) = s == "Q"
>	prefer_Q l r = if is_Q l then LT else if is_Q r then GT else EQ

%----------------------------------------------------------------------------
Test values

> p n = Pred n []
> eg1 
>  = [ Clause [p "A"] [p "B"] , Clause [p "A"] [p "D"]
>    , Clause [p "B"] [p "C"] , Clause [p "D"] [p "C"]
>    , Clause [p "B"] [p "C"] , Clause [p "C"] []
>    , Clause []      [p "A"]
>    ]
