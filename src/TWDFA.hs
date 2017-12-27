{-# LANGUAGE RecordWildCards #-}
module TWDFA where

import           Control.Arrow (second)
import           Data.List     (inits)

data TWDFA s a = TWDFA
  { trans  :: s -> WithEndmark a -> (s, Dir)
  , start  :: s
  , accept :: s
  , reject :: s
  }

data Dir = L | R
  deriving Show

data WithEndmark alphabet
  = LeftEnd
  | Inner alphabet
  | RightEnd
  deriving Show

runTWDFA :: TWDFA s a -> [a] -> [(s, Int)]
runTWDFA (TWDFA {..}) cs = map (second (length . fst)) $
  flip iterate (start, ([], [LeftEnd] ++ map Inner cs ++ [RightEnd])) $
  \(q, (ls'@(~(l:ls)), r:rs))-> case trans q r of
    (q', R) -> (q', (r:ls', rs))
    (q', L) -> (q', (ls, l:r:rs))

runTWDFA' :: Eq s => TWDFA s a -> [a] -> [(s, Int)]
runTWDFA' twdfa@(TWDFA {..}) cs = head $ dropWhile (flip notElem [accept, reject] . fst . last) $ tail $ inits $ runTWDFA twdfa cs

data States
  = Qt | Qr
  | Qq0 | Qq1 | Qq2
  | Qp0 | Qp1
  deriving (Eq, Show)

data CharAB = A | B
  deriving Show

example1 :: TWDFA States CharAB
example1 = TWDFA trans Qq0 Qt Qr
  where
  -- q0
  trans Qq0 LeftEnd   = (Qq0, R)
  trans Qq0 (Inner A) = (Qq1, R)
  trans Qq0 (Inner B) = (Qq0, R)
  trans Qq0 RightEnd  = (Qp0, L)
  -- q1
  trans Qq1 (Inner A) = (Qq2, R)
  trans Qq1 (Inner B) = (Qq1, R)
  trans Qq1 RightEnd  = (Qr , L)
  -- q2
  trans Qq2 (Inner A) = (Qq0, R)
  trans Qq2 (Inner B) = (Qq2, R)
  trans Qq2 RightEnd  = (Qr , L)
  -- p0
  trans Qp0 LeftEnd   = (Qt , R)
  trans Qp0 (Inner A) = (Qp0, L)
  trans Qp0 (Inner B) = (Qp1, L)
  -- p1
  trans Qp1 LeftEnd   = (Qr , R)
  trans Qp1 (Inner A) = (Qp1, L)
  trans Qp1 (Inner B) = (Qp0, L)
  -- t
  trans Qt RightEnd   = (Qt, L)
  trans Qt _          = (Qt, R)
  -- others
  trans _ RightEnd    = (Qr, L)
  trans _ _           = (Qr, R)
