module Arith.Syntax.Utils (spanCount, peek, peekNext) where

import Data.Char (isSpace)
import Data.List (foldl')


spanCount :: [a] -> (a -> Bool) -> ([a], [a], Integer)
spanCount [] _ = ([], [], 0)
spanCount [c] p =
  let matched = p c
   in ([c | matched], [], if matched then 1 else 0)
spanCount source p =
  let (matching, rest) = span p source
   in (matching, rest, fromIntegral $ length matching)


peek :: [a] -> Maybe a
peek (c : _) = Just c
peek [] = Nothing


peekNext :: [a] -> Maybe a
peekNext (_ : c : _) = Just c
peekNext _ = Nothing
