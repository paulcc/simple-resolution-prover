> module ListSubstitution
>   ( Substitution(..)
>   , ListSubstitution
>   , emptyListSubstitution        -- constant, useful for avoiding casts
>   ) where

Simple implementation of `Substitution' - via table of pairs

> import Data.List(union, intersect)
> import Text.PrettyPrint(char, text, (<+>), brackets, comma, cat, (<>))

> import TermStuff


%----------------------------------------------------------------------------

> data ListSubstitution = LS [(VarName, Term)]

> emptyListSubstitution = LS []

---

> instance ContainsVars ListSubstitution where
>   vars_in (LS as) = vars_in (map snd as)  `union` map fst as

> instance Substitution ListSubstitution where
>   emptySubstitution = emptyListSubstitution
>
>   lookup_var (LS as) v = lookup v as
>
>   extend_with = my_extend_with
>
>   v +-> t  | v `elem` vars_in t = error $ "Circularity in substitution"
>            | otherwise          = LS [(v,t)]


%---------------------------------------
`my_extend_with' 
  - it deliberately fails on range overlap
  - it deliberately fails if old subst maps vars in new subst
  - it expects that old subst is applied before unification produces 
    the new subst.
  - the idea is that the new substitutions solves some vars in the 
    range terms of the old subst, representing a progress in knowledge,
    _NOT_ the other way round

> (LS old_vts) `my_extend_with` new@(LS new_vts)
>  | not $ null $ map fst old_vts `intersect` map fst new_vts
>  = error "Substitution domains overlap"
>
>  | not $ null $ fmap fst old_vts `intersect` vars_in (map snd new_vts)
>  = error $ "Old vars appear in new substitution"
>
>  | otherwise
>  = LS $ [ (v, apply new t) | (v,t) <- old_vts ] ++ new_vts

%---------------------------------------
Appearance of ListSubstitutions 

> instance PrettyPrintable ListSubstitution where
>   pp (LS [])     = text "[]"
>   pp (LS [x])    = brackets $ pp_item x
>   pp (LS (x:xs)) = cat (  char '[' <> pp_item x
>                        :  [ comma <> pp_item x | x <- xs ]
>                        ++ [text "]"])

> pp_item (v,t) = pp t <+> text "/" <+> pp v


