module TTG where

import TypeTheory
import Semantics
import Lang
import PGF
import ReadDictionary

import qualified TT as TT
import TTG2TT

main = do
  pgf <- readPGF "Lang.pgf"
  s <- getContents 
  genlist <- extractGenders "DictEng.gf"
  let (s1,s2) = (interpret genlist pgf s)
  putStrLn s1
  TT.writeTPTP "q.p" (init s2) (last s2)
   
interpret :: GenderList -> PGF -> String -> (String,[TT.FO])
interpret genlist pgf s =
  let
    phrs   = lines s
    treess = map (parse pgf (mkCId "LangEng") (startCat pgf)) phrs
    conts  = map ((iPhrs genlist) . map fg) (sequence treess) ---- should not be expanded this way
    forms  = map prContext conts
    fols   = [[(x,TT.App x [] TT.-: ttg2tt p) | (x,p) <- co] | co <- conts]
    tptps  = [[(x,p) | (x,p) <- co] | co <- fols]    
  in (unlines $ [ s ++ "\n->TT: " ++ p ++ "->FO: " ++ unlines (map (show . snd) f) ++ "->GF: " ++ show (fg t :: GPhr) ++ "\n"
                | (p,(f,(s,t))) <- forms `zip` (fols `zip` [ (s,t) | (s,ts) <- phrs `zip` treess, t <- ts ]) ],
       map snd (concat tptps))





