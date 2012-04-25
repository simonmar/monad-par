{-# LANGUAGE CPP #-}
-- Time-stamp: <2011-02-12 21:11:31 simonmar>
-----------------------------------------------------------------------------

module Game where

import Board
import Tree

import Control.Parallel
import Debug.Trace
import qualified Control.Monad.Par.Combinator as C
#ifdef PARSCHED
import PARSCHED
#else
import Control.Monad.Par (runPar, Par)
#endif

type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)

alternate :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternate _ _ _ _ b | fullBoard b = []
alternate _ _ _ _ b | static b == XWin = []
alternate _ _ _ _ b | static b == OWin = []
alternate depth player f g board = move : alternate depth opponent g f board'
  where
    move@(board',eval) = best f possibles scores
    scores = runPar $ C.parMapM (bestMove depth opponent g f) possibles
    possibles = newPositions player board
    opponent = opposite player

alternateNested :: Int -> Piece -> Player -> Player -> Board -> [Move]
alternateNested _ _ _ _ b | fullBoard b = []
alternateNested _ _ _ _ b | static b == XWin = []
alternateNested _ _ _ _ b | static b == OWin = []
alternateNested depth player f g board = move : alternateNested depth opponent g f board'
  where
    move@(board',eval) = best f possibles scores
    scores = runPar $ C.parMap (bestMoveNested depth opponent g f) possibles
    possibles = newPositions player board
    opponent = opposite player

opposite :: Piece -> Piece
opposite X = O
opposite O = X


best :: Player -> [Board] -> [Evaluation] -> Move
best f (b:bs) (s:ss) = best' b s bs ss
	where
	best' b s [] [] = (b,s)
	best' b s (b':bs) (s':ss) | s==(f s s') = best' b s bs ss
				  | otherwise 	= best' b' s' bs ss

showMove :: Move -> String
showMove (b,e) = show e ++ "\n" ++ showBoard b

bestMove :: Int -> Piece -> Player -> Player -> Board -> Par Evaluation
bestMove depth p f g board
  = do
    let tree = cropTree $ mapTree static $ prune depth $ searchTree p $ board
    parMise 2 f g tree

bestMoveNested :: Int -> Piece -> Player -> Player -> Board -> Evaluation
bestMoveNested depth p f g board
  = let tree = cropTree $ mapTree static $ prune depth $ searchTree p $ board
    in runPar $ parMise 2 f g tree

cropTree :: (Tree Evaluation) -> (Tree Evaluation)
cropTree (Branch a []) = (Branch a [])
cropTree (Branch (Score x) l) = Branch (Score x) (map cropTree l)
cropTree (Branch x l) = Branch x []

searchTree :: Piece -> Board -> (Tree Board)
searchTree p board = repTree (newPositions p) (newPositions (opposite p)) board

mise :: Player -> Player -> (Tree Evaluation) -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr f (g OWin XWin) (map (mise g f) l)

parMise :: Int -> Player -> Player -> (Tree Evaluation) -> Par Evaluation
parMise 0 f g t = return (mise f g t)
parMise n f g (Branch a []) = return a
parMise n f g (Branch _ l) = do
  es <- C.parMapM (parMise (n-1) g f) l
  return (foldr f (g OWin XWin) es)
