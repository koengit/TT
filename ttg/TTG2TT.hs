module TTG2TT where

import TypeTheory
import qualified TT as TT

import Control.Monad.State
import Data.List
import Data.Char

ttg2tt :: Prop -> TT.TT
ttg2tt p = fst $ runState (prp p) initTTGState where
  prp p = case p of
    Sigma p f -> do
      sp <- prp p
      x  <- useNextvar
      sf <- prp (f (Var x))
      return $ TT.Sigma x sp sf
    Pi p f -> do
      sp <- prp p
      x  <- useNextvar
      sf <- prp (f (Var x))
      return $ TT.Pi x sp sf
    Atom f xs -> do
      return $ TT.Basic (trans f) (map ttInd xs)
     where trans s 
                | "_N" `isSuffixOf` s = toUpper (head s):(tail s)
                | otherwise = s
    Conj p q -> do
      sp <- prp p
      sq <- prp q
      x  <- useNextvar --- or dummyName
      return $ TT.Sigma x sp sq
    Impl p q -> do
      sp <- prp p
      sq <- prp q
      x  <- useNextvar --- or dummyName
      return $ TT.Pi x sp sq
    Disj p q -> do
      sp <- prp p
      sq <- prp q
      return $ TT.Or sp sq
    Neg q -> do
      sq <- prp q
      x  <- useNextvar --- or dummyName
      return $ TT.Pi x sq TT.Bottom
    Falsum -> return TT.Bottom

ttInd :: Ind -> TT.Term
ttInd i = case i of
   Const f xs -> TT.App (trans f) (map ttInd xs)
   Def   f xs -> TT.App "DEF"  (map ttInd xs)
   Pron  p xs -> TT.App "PRON" (map ttInd xs)
   Var x -> TT.Var x
  where
    trans "p" = "$p"
    trans "q" = "$q"
    trans "ap" = "$ap"
    trans s   = s

