module TTG where

import Semantics
import Lang
import PGF

main = do
  pgf <- readPGF "Lang.pgf"
  interact (interpret pgf)

interpret :: PGF -> String -> String
interpret pgf s =
  let
    phrs   = lines s
    treess = map (parse pgf (mkCId "LangEng") (startCat pgf)) phrs
    conts  = map (iPhrs . map fg) (sequence treess) ---- should not be expanded this way
    forms  = map prContext conts
  in unlines forms


{-

prop2fol :: Prop -> Prop
prop2fol p =

f : (Pi z : (Sigma x : A)B(x))C(p(z),q(z))

All z . [ z : Sigma... ] => [ ap(f,z) : C... ]
        [p z : A ] & [ q z : B pz ] 

-- Koen 2/3/2017
The translation has the type:

  [| _ : _ |] : (Proof, Type) -> Formula

Where Proof is a proof term, Type is a type, and Formula is a first-order formula.

The two main translations are:

  [| f : (PI x : A)B(x) |] = All x . ([| x : A |] => [| ap(f,x) : B(x) |])

and

  [| c : (SIGMA x : A)B(x) |] = [| p(c) : A |] & [| B(q(p(c))) |]
-}
