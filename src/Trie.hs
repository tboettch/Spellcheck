
module Trie (
  Word,
  sanitize,
  Dictionary,
  empty,
  insert,
  contains
) where

import Data.Array
import Data.Char
import qualified Data.Map as Map


data Word = Word String deriving (Show, Eq)

sanitize :: String -> Word
sanitize = Word . (map toLower) . (filter isAlpha)

data Node = Dense Bool (Array Char (Maybe Node))
          | Sparse Bool (Map.Map Char Node)
          deriving (Show, Eq)

data Dictionary = Dict Node deriving (Show, Eq)

empty :: Dictionary
empty = Dict $ Dense False (array ('a', 'z') (map (\c -> (c, Nothing)) ['a'..'z']))

insert :: Word -> Dictionary -> Dictionary
insert (Word s) (Dict node) = Dict $ go 0 s node
  where go :: Int -> String -> Node -> Node
        go _ [] n@(Dense True _)  = n
        go _ [] (Dense False arr) = Dense True arr
        go _ [] n@(Sparse True _) = n
        go _ [] (Sparse False m)  = Sparse True m
        go d (x:xs) (Dense bool arr) = Dense bool $ case arr ! x of
                                         Nothing  -> arr // [(x, Just $ finish (d+1) xs)]
                                         (Just n) -> arr // [(x, Just $ go     (d+1) xs n)]
        go d (x:xs) (Sparse bool m)  = Sparse bool $ case Map.lookup x m of
                                                      Nothing  -> Map.insert x (finish (d+1) xs) m
                                                      (Just n) -> Map.insert x (go (d+1) xs n) m
        finish :: Int -> String -> Node
        finish d []     | d < cutoff = Dense True $ array ('a', 'z') (map (\c -> (c, Nothing)) ['a'..'z'])
                        | otherwise  = Sparse True $ Map.empty
        finish d (x:xs) | d < cutoff = Dense False $ array ('a', 'z') (map (\c -> (c, if c == x then Just $ finish (d+1) xs else Nothing)) ['a'..'z'])
                        | otherwise  = Sparse False $ Map.singleton x (finish (d+1) xs)
        cutoff = 4

contains :: Word -> Dictionary -> Bool
contains (Word s) (Dict node) = go s node
  where go [] (Dense bool _)    = bool
        go [] (Sparse bool _)   = bool
        go (x:xs) (Dense _ arr) = case arr ! x of
                                    Nothing      -> False
                                    (Just node') -> go xs node'
        go (x:xs) (Sparse _ m)  = case Map.lookup x m of
                                    Nothing -> False
                                    (Just node')  -> go xs node'
        
