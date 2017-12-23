{-# LANGUAGE RecordWildCards #-}
module TWDFA where

import           Control.Arrow   (second)
import           Data.List       (inits)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data TWDFA states alphabet = TWDFA
  { trans  :: Map (states, WithEndmark alphabet) (states, Dir)
  , start  :: states
  , accept :: states
  , reject :: states
  } deriving (Show)

data Dir = L | R
  deriving (Eq, Show)

data WithEndmark alphabet
  = LeftEnd
  | Inner alphabet
  | RightEnd
  deriving (Eq, Ord, Show)

data States
  = Qs | Qt | Qr
  | Qa0 | Qa1
  | Qb0 | Qb1 | Qb2
  deriving (Eq, Ord, Show)

data CharAB = A | B
  deriving (Eq, Ord, Show)

sample1 :: TWDFA States CharAB
sample1 = TWDFA trans1 Qs Qt Qr
  where
  trans1 = Map.fromList [
    ((Qs , LeftEnd ), (Qa0, R)),
    ((Qa0, Inner A ), (Qa1, R)),
    ((Qa1, Inner A ), (Qa0, R)),
    ((Qa0, Inner B ), (Qa0, R)),
    ((Qa1, Inner B ), (Qa1, R)),
    ((Qa0, RightEnd), (Qb0, L)),
    ((Qa1, RightEnd), (Qr , L)),
    ((Qb0, Inner B ), (Qb1, L)),
    ((Qb1, Inner B ), (Qb2, L)),
    ((Qb2, Inner B ), (Qb0, L)),
    ((Qb0, Inner A ), (Qb0, L)),
    ((Qb1, Inner A ), (Qb1, L)),
    ((Qb2, Inner A ), (Qb2, L)),
    ((Qb0, LeftEnd ), (Qt , R)),
    ((Qb1, LeftEnd ), (Qr , R)),
    ((Qb2, LeftEnd ), (Qr , R))]

runTWDFA :: (Ord states, Ord alphabet) => TWDFA states alphabet -> [alphabet] -> [(states, Int)]
runTWDFA (TWDFA {..}) cs = map (second (length . fst)) $ iterate f (start, ([], [LeftEnd] ++ map Inner cs ++ [RightEnd]))
  where
  f (q, (ls'@(~(l:ls)), r:rs)) = case Map.lookup (q, r) trans of
    Just (q', R) -> (q', (r:ls', rs))
    Just (q', L) -> (q', (ls, l:r:rs))
    Nothing      -> (reject, (ls, r:rs))

runTWDFA' :: (Ord states, Ord alphabet) => TWDFA states alphabet -> [alphabet] -> [(states, Int)]
runTWDFA' twdfa@(TWDFA {..}) cs = head $ dropWhile (flip notElem [accept, reject] . fst . last) $ tail $ inits $ runTWDFA twdfa cs
