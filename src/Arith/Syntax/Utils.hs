module Arith.Syntax.Utils (spanCount, peek, peekNext) where

import Data.Char (isSpace)


spanCount :: [a] -> (a -> Bool) -> ([a], [a], Integer)
spanCount [] _ = ([], [], 0)
spanCount [c] p =
  let matched = p c
   in ([c | matched], [], if matched then 1 else 0)
spanCount source p = sliceCount source p ([], source, 0)
 where
  sliceCount xs p acc@(matches, nonMatching, n)
    | null xs = acc
    | otherwise =
        let (x : xs') = xs
         in if p x
              then
                sliceCount
                  xs'
                  p
                  ( matches ++ [x]
                  , xs'
                  , n + 1
                  )
              else acc


peek :: [a] -> Maybe a
peek (c : _) = Just c
peek [] = Nothing


peekNext :: [a] -> Maybe a
peekNext (_ : c : _) = Just c
peekNext _ = Nothing
