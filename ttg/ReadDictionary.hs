module ReadDictionary where

import Data.List.Split
import System.IO

extractGenders :: FilePath -> IO [(String,String)]
extractGenders f = do
 s <- readFile f
 let 
     ss   = lines s
     toks = filter (\tks -> head tks == "lin" && length tks > 4) [splitOn " " row | row <- ss, not (null row)]
 return [ (t !! 1, let gen = (t !! 4) in if gen == "masculine" then "Masc" 
                                           else if gen == "feminine" then "Fem" else "Hum" ) 
          | t <- toks, (t !! 4)  `elem` ["masculine","feminine", "human"]]
    
     
 
