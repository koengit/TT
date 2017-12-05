module TypeTheory where

import Control.Monad.State
import Data.List

-- abstract syntax (HOAS)

data Prop =
    Sigma Prop (Ind -> Prop)
  | Pi Prop (Ind -> Prop)
  | Atom Ident [Ind]
  | Disj Prop Prop
  | Conj Prop Prop
  | Impl Prop Prop
  | Neg Prop
  | Falsum

data Ind =
    Const Ident [Ind]
  | Var Ident
  | Def Prop [Ind]   --- interpretation of definite phrase
  | Pron Gender [Ind] --- interpretation of pronoun

type Ident = String
type Context = [(Ident,Prop)]

type Gender = String ---- language-specific; for pronouns

-- printing

data TTGState = TTGState {
  nextvar :: Int
  }

initTTGState = TTGState 0

useNextvar :: State TTGState Ident
useNextvar = do
  i <- gets nextvar
  let x = "x" ++ show i
  modify (\s -> s{nextvar = i+1})
  return x

prContext :: Context -> String
prContext rps = unlines [x ++ " : " ++ prProp p | (x,p) <- rps]

prProp :: Prop -> String
prProp p = fst $ runState (prp p) initTTGState where
  prp p = case p of
    Sigma p f -> do
      sp <- prp p
      x  <- useNextvar
      sf <- prp (f (Var x))
      return $ concat $ [parenth ("Σ" ++ x ++ " : " ++ sp),sf]
    Pi p f -> do
      sp <- prp p
      x  <- useNextvar
      sf <- prp (f (Var x))
      return $ concat $ [parenth ("Π" ++ x ++ " : " ++ sp),sf]
    Atom f xs -> do
      return $ f ++ case xs of
        [] -> ""
        _ -> parenth $ concat $ intersperse "," $ map prInd xs
    Conj p q -> do
      sp <- prp p
      sq <- prp q
      return $ parenth $ unwords [sp,"&",sq] 
    Impl p q -> do
      sp <- prp p
      sq <- prp q
      return $ parenth $ unwords [sp,"->",sq] 
    Disj p q -> do
      sp <- prp p
      sq <- prp q
      return $ parenth $ unwords [sp,"+",sq] 
    Neg q -> do
      sq <- prp q
      return $ unwords ["~",sq] 
    Falsum -> return "⊥"

prInd :: Ind -> String
prInd i = case i of
   Const f xs -> f ++ case xs of
     [] -> ""
     _ -> parenth $ concat $ intersperse "," $ map prInd xs
   Def  f xs -> "DEF"  ++ (parenth $ concat $ intersperse "," $ (prProp f : map prInd xs))
   Pron p xs -> "PRON" ++ (parenth $ concat $ intersperse "," $ (       p : map prInd xs))
   Var x -> x

parenth s = "(" ++ s ++ ")"

