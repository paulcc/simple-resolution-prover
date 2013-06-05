> module Gtree_search where


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Synonyms and interface for searching.

> type OpenList a = [a]
> type SearchFn s = OpenList s -> Maybe (s, OpenList s)

> type Goal_Predicate s      = s -> Bool
> type Successor_Generator s = s -> [s]

> class Searchable s where
>   is_goal    :: Goal_Predicate s
>   successors :: Successor_Generator s



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
A Gtree type.

> data Gtree a = Gtree { root_item :: a
>                      , children  :: [Gtree a] }
>                deriving (Show, Eq)

---
Folding a Gtree
 - might be useful later on.

> fold_Gtree :: (a -> [b] -> b) -> Gtree a -> b
> fold_Gtree nf (Gtree a cs) = nf a [ fold_Gtree nf c | c <- cs ]

---
`state_space'
  - build a Gtree by repeatedly using successor fn

> state_space :: Successor_Generator s -> s -> Gtree s 
> state_space succ_fn s 
>  = Gtree s [state_space succ_fn c | c <- succ_fn s ]




%----------------------------------------------------------------------------
Traversals.

(explicit) version of depth first traversal
 - visit the root item first,  then do depth first traversals on each child
   (from left to right).

> depth_first :: Gtree a -> [a]
> depth_first (Gtree n cs) = n : concat (map depth_first cs)

---
(explicit) version of breadth first traversal
 - maintains a list of the trees at the current level
 - skims off the root items from these, 
 - then does bf on the collected children of these trees
 - imagine it going through the tree in layers...

> breadth_first :: Gtree a -> [a]
> breadth_first t
>  = bf [t]
>    where 
>        bf [] = []
>        bf ts = map root_item ts ++ bf (concat $ map children ts)


---
Best-first traversal of a Gtree. 
  - this version uses recursion and laziness in a subtle way.
  - you might like to try to understand why it works.

> best_first :: (a -> a -> Ordering) -> Gtree a -> [a]
> best_first cmp (Gtree a cs)
>  = a : foldr (mergeBy cmp) [] (map (best_first cmp) cs)

---
Merge two lists using the ordering relation
 - aux fn - this needs to go in other module - eg util fns.

> mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
> mergeBy cmp [] bs = bs
> mergeBy cmp as [] = as
> mergeBy cmp (a:as) (b:bs) 
>  = case (cmp a b) of
>      GT -> b : mergeBy cmp (a:as) bs
>      _  -> a : mergeBy cmp as     (b:bs)


%----------------------------------------------------------------------------
Searching
 - can build a search space (as tree) and traverse it in various ways
 - it remains to filter out the goal states from the traversal list


> search_the_tree :: Searchable s => (Gtree s -> [s]) -> s -> [s]
> search_the_tree traversal_fn s0
>  = [ s | s <- traversal_fn $ state_space successors s0, is_goal s ]

> search_trace :: Searchable s => (Gtree s -> [s]) -> s -> [Either s s]
> search_trace traversal_fn s0
>  = [ if is_goal s then Right s else Left s
>    | s <- traversal_fn $ state_space successors s0 ]

> new_dfs :: Searchable s => s -> [s]
> new_dfs = search_the_tree depth_first 

> new_brfs :: Searchable s => s -> [s]
> new_brfs = search_the_tree breadth_first 

> new_bestfs :: Searchable s => (s -> s -> Ordering) -> s -> [s]
> new_bestfs cmp = search_the_tree (best_first cmp)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Utils.

> display_gt :: Show a => Gtree a -> IO ()
> display_gt 
>  = putStr . st 0 
>    where
>       indent i s = replicate i ' ' ++ s
>       st i (Gtree a cs) 
>        = indent i (show a) ++ "\n" ++ concat (map (st (i+2)) cs)


%----------------------------------------------------------------------------
Size functions.

> size_Gtree :: Gtree a -> Int
> size_Gtree = fold_Gtree (\_ cs -> 1 + sum cs)

> size_est :: Successor_Generator s -> s -> Int
> size_est sf = size_Gtree . state_space sf



%----------------------------------------------------------------------------

> t1 = (Gtree 1 [ Gtree 3 [Gtree 6 [Gtree 7 []], 
>                          Gtree 5 [Gtree 4 []]], 
>                 Gtree 2 [Gtree 8 []]])

> tb = (Gtree 1 [ Gtree 3 [Gtree 2 [Gtree 7 []], 
>                          Gtree 5 [Gtree 4 []]], 
>                 Gtree 2 [Gtree 6 [Gtree 0 []]]])

> tc = (Gtree 1 [ Gtree 3     [Gtree (2.0) [Gtree 7 []]
>                             ,Gtree (2.5) [Gtree 4 []]] 
>               , Gtree (4.1) [Gtree (7.5) [Gtree 0 []]]])

