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

runTWDFA :: (Ord states, Ord alphabet) => TWDFA states alphabet -> [alphabet] -> [(states, Int)]
runTWDFA (TWDFA {..}) cs = map (second (length . fst)) $ iterate f (start, ([], [LeftEnd] ++ map Inner cs ++ [RightEnd]))
  where
  f (q, (ls'@(~(l:ls)), r:rs)) = case Map.lookup (q, r) trans of
    Just (q', R) -> (q', (r:ls', rs))
    Just (q', L) -> (q', (ls, l:r:rs))
    Nothing      -> (reject, (ls, r:rs))

runTWDFA' :: (Ord states, Ord alphabet) => TWDFA states alphabet -> [alphabet] -> [(states, Int)]
runTWDFA' twdfa@(TWDFA {..}) cs = head $ dropWhile (flip notElem [accept, reject] . fst . last) $ tail $ inits $ runTWDFA twdfa cs

data States
  = Qt | Qr
  | Qq0 | Qq1 | Qq2
  | Qp0 | Qp1
  deriving (Eq, Ord, Show)

data CharAB = A | B
  deriving (Eq, Ord, Show)

sample1 :: TWDFA States CharAB
sample1 = TWDFA trans1 Qq0 Qt Qr
  where
  trans1 = Map.fromList [
    ((Qq0, LeftEnd), (Qq0, R)), ((Qq0, Inner A), (Qq1, R)), ((Qq0, Inner B), (Qq0, R)), ((Qq0, RightEnd), (Qp0, L)),
                                ((Qq1, Inner A), (Qq2, R)), ((Qq1, Inner B), (Qq1, R)), ((Qq1, RightEnd), (Qr , L)),
                                ((Qq2, Inner A), (Qq0, R)), ((Qq2, Inner B), (Qq2, R)), ((Qq2, RightEnd), (Qr , L)),
    ((Qp0, LeftEnd), (Qt , R)), ((Qp0, Inner A), (Qp0, L)), ((Qp0, Inner B), (Qp1, L)),
    ((Qp1, LeftEnd), (Qr , R)), ((Qp1, Inner A), (Qp1, L)), ((Qp1, Inner B), (Qp1, L))]
