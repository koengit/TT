module Semantics where

import Lang
import TypeTheory

import Control.Monad.State
import Data.List

-- resolve anaphora

resolve :: Context -> Prop -> Prop
resolve ctx = res [(Const r [],p) | (r,p) <- ctx] -- initialize with text context
 where
  res :: [(Ind,Prop)] -> Prop -> Prop
  res xs p = case p of
    Sigma p f -> let rp = res xs p in Sigma rp (\x -> res ((x,rp) : xs) (f x))
    Pi    p f -> let rp = res xs p in Pi    rp (\x -> res ((x,rp) : xs) (f x))
    Atom f ys -> Atom f $ map (resInd xs) ys
    Conj a b  -> Conj (res xs a) (res xs b)
    _ -> p
  resInd :: [(Ind,Prop)] -> Ind -> Ind
  resInd xs i = case i of
    Const f ts -> Const f (map (resInd xs) ts)
    Def  typ [] -> ifUnique (Def typ)  [px x | (x,t) <- xs, px <- refsDef  typ t]
    Pron pro [] -> ifUnique (Pron pro) [px x | (x,t) <- xs, px <- refsPron pro t]
    _ -> i
  ifUnique f xs = case xs of
    [x] -> x
    _ -> f xs

refsDef :: Prop -> Prop -> [Ind -> Ind]
refsDef sought given = seek 0 sought given where
  seek i s p = case (s,p) of
    (Atom a [], Atom b []) | a==b -> [id]
    (_,         Sigma b f) -> [compP h | h <- seek i s b ] ++ [compQ h | h <- seek (i+1) s (f (Var (show i)))]
    _ -> []

compP h = \x -> h (Const "p" [x])
compQ h = \x -> h (Const "q" [x])
    
refsPron :: Gender -> Prop -> [Ind -> Ind]
refsPron sought given = refsDef (Atom sought []) (typeToGenders given)

---- TODO: get this language-dependent definition from the grammar
typeToGenders :: Prop -> Prop
typeToGenders p = case p of
  Atom a []
    | elem a ["woman_N","girl_N","mother_N","daughter_N"] -> Atom "Fem" []
    | elem a ["man_N","boy_N","father_N","son_N"] -> Atom "Masc" []
    | otherwise -> Atom "Neutr" []
  Sigma a f -> Sigma (typeToGenders a) (\x -> typeToGenders (f x))
  _ -> p  --- no references outside atom and sigma

-- interpret a text as a list of phrases
iPhrs :: [GPhr] -> Context
iPhrs ps = res [] (zip ["r" ++ show i | i <- [0..]] ps)
 where
  res ctx rps = case rps of
    (r,p) : ps -> let p' = resolve ctx (iPhr p) in (r,p') : res ((r,p'):ctx) ps
    _ -> []

notyet :: Show a => a -> b
notyet s = error $ "not yet: " ++ show s

-- interpretations of RGL Lang.gf 

iPhr :: GPhr -> Prop
iPhr p = case p of
  GPhrUtt _ utt _ -> iUtt utt

iUtt :: GUtt -> Prop
iUtt u = case u of
  GUttS s -> iS s
  _ -> notyet u

iS :: GS -> Prop
iS s = case s of
  GUseCl _ pol cl -> iPol pol (iCl cl)
  GConjS Gand_Conj (GListS (s:ss)) -> Sigma (iS s) (\x -> iS (case ss of [t] -> t ; _ -> GConjS Gand_Conj (GListS ss)))
  GAdvS (GSubjS Gif_Subj a) b -> Pi (iS a) (\x -> iS b) --- non-compositional
  GConjS c (GListS ss) -> foldl1 (iConj c) (map iS ss)
  _ -> notyet s

iQS :: GQS -> Prop ---- TODO proper type
iQS qs = case qs of
  GUseQCl _ pol cl -> iPol pol (iQCl cl)

iRS :: GRS -> Ind -> Prop
iRS rs x = case rs of
  GUseRCl _ pol rcl -> iPol pol (iRCl rcl x)
  _ -> notyet rs

iPol :: GPol -> Prop -> Prop
iPol p s = case p of
  GPPos -> s
  GPNeg -> Neg s

iCl :: GCl -> Prop
iCl s = case s of
  GPredVP np vp   -> iNP np (iVP vp)
  GPredSCVP sc vp -> Sigma (iSC sc) (iVP vp)  --- sentence subject by existential 
  _ -> notyet s

iQCl :: GQCl -> Prop
iQCl s = case s of
  GQuestCl cl   -> iCl cl
  _ -> notyet s

iRCl :: GRCl -> Ind -> Prop
iRCl rcl x = case rcl of
  GRelVP rp vp -> iVP vp x ---- TODO: RP
  GRelSlash rp cls -> iClSlash cls x
  _ -> notyet rcl

iClSlash :: GClSlash -> Ind -> Prop
iClSlash s x = case s of
  GSlashVP np vps -> iNP np (\y -> iVPSlash vps y x)
  _ -> notyet s

iSC :: GSC -> Prop
iSC sc = case sc of
  GEmbedS  s  -> iS s
  GEmbedQS qs -> iQS qs
  _ -> notyet sc
  
iVP :: GVP -> Ind -> Prop
iVP vp x = case vp of
  GComplSlash vps np -> iNP np (\y -> iVPSlash vps x y)
  GUseV (LexV v) -> Atom v [x]
  GAdvVP vp adv -> Sigma (iVP vp x) (\y -> iAdv adv y)
  GUseComp comp -> iComp comp x
  GReflVP vps -> iVPSlash vps x x
  _ -> notyet vp

iVPSlash :: GVPSlash -> Ind -> Ind -> Prop
iVPSlash vp x y = case vp of
  GSlashV2a (LexV2 v) -> Atom v [x,y]

iComp :: GComp -> Ind -> Prop
iComp comp x = case comp of
  GCompAP ap -> iAP ap x
  _ -> notyet comp

iNP :: GNP -> (Ind -> Prop) -> Prop
iNP np p = case np of
  GDetCN (GDetQuant GDefArt _) cn -> p (Def (iCN cn) [])
  GDetCN det cn -> iDet det (iCN cn) p
  GConjNP c (GListNP nps) -> foldl1 (iConj c) (map (flip iNP p) nps)
  GUsePron r -> p (Pron (iPron r) [])
  GUsePN (LexPN s) -> p (Const s [])

-- reducing pronoun to gender ---- English-specific
iPron :: GPron -> Gender
iPron p = case p of
  Ghe_Pron  -> "Masc"
  Gshe_Pron -> "Fem"
  Git_Pron  -> "Neutr"

iDet :: GDet -> Prop -> (Ind -> Prop) -> Prop
iDet det t p = case det of
  GsomeSg_Det -> Sigma t p
  Gevery_Det -> Pi t p
  GDetQuant GIndefArt _ -> Sigma t p --- non-compositional
  
iCN :: GCN -> Prop
iCN cn = case cn of
  GRelCN cn rs -> Sigma (iCN cn) (\x -> iRS rs x) 
  GAdjCN ap cn -> Sigma (iCN cn) (\x -> iAP ap x)
  GApposCN cn np -> iCN cn ---- TODO: semantics of apposition
  GUseN (LexN n) -> Atom n []
  _ -> notyet cn

iAP :: GAP -> Ind -> Prop
iAP ap x = case ap of
  GPositA (LexA a) -> Atom a [x]
  GConjAP c (GListAP aps) -> foldl1 (iConj c) (map (flip iAP x) aps)

iAdv :: GAdv -> Ind -> Prop
iAdv adv e = case adv of
  LexAdv s  -> Atom s [e]
----  GPrepNP prep np -> iPrep prep (iNP np) e

----iPrep :: GPrep -> ((Ind -> Prop) -> Prop) -> Ind -> Prop

iConj :: GConj -> Prop -> Prop -> Prop
iConj c x y = case c of
  Gand_Conj -> Conj x y
  Gor_Conj -> Disj x y

