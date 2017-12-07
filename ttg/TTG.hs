module TTG where

import TypeTheory
import Semantics
import Lang
import PGF

import qualified TT as TT
import TTG2TT

main = do
  pgf <- readPGF "Lang.pgf"
  s <- getContents 
  let (s1,s2) = (interpret pgf s)
  putStrLn s1
  TT.writeTPTP "q.p" (init s2) (last s2)
   
interpret :: PGF -> String -> (String,[TT.FO])
interpret pgf s =
  let
    phrs   = lines s
    treess = map (parse pgf (mkCId "LangEng") (startCat pgf)) phrs
    conts  = map (iPhrs . map fg) (sequence treess) ---- should not be expanded this way
    forms  = map prContext conts
    fols   = [[(x,TT.App x [] TT.-: ttg2tt p) | (x,p) <- co] | co <- conts]
    tptps  = [[(x,p) | (x,p) <- co] | co <- fols]    
  in (unlines $
         forms 
      ++ map (show . snd) (concat fols),
       map snd (concat tptps))





