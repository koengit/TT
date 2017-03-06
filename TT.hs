module TT where

import Data.List( union, (\\), intercalate )

type Name
  = String

data Term
  = App Name [Term]
  | Var Name
 deriving ( Eq, Ord )

instance Show Term where
  show (App f []) = f
  show (App f xs) = f ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Var x)    = x

data TT
  = Pi Name TT TT
  | Sigma Name TT TT
  | Basic Name [Term]
 deriving ( Eq, Ord )

instance Show TT where
  show (Pi    x a b) = "(Π" ++ x ++ ":" ++ show a ++ ")" ++ show b
  show (Sigma x a b) = "(Σ" ++ x ++ ":" ++ show a ++ ")" ++ show b
  show (Basic h [])  = h
  show (Basic h xs)  = h ++ "(" ++ intercalate "," (map show xs) ++ ")"

data FO
  = All Name FO
  | Exi Name FO
  | FO :&: FO
  | FO :=>: FO
  | Atom Name [Term]
 deriving ( Eq, Ord )

instance Show FO where
  show (All x p)   = "∀" ++ x ++ "." ++ show p
  show (Exi x p)   = "∃" ++ x ++ "." ++ show p
  show (p :&: q)   = "(" ++ show p ++ " & " ++ show q ++ ")"
  show (p :=>: q)  = "(" ++ show p ++ " => " ++ show q ++ ")"
  show (Atom h []) = h
  show (Atom h xs) = h ++ "(" ++ intercalate "," (map show xs) ++ ")"

app :: Term -> Term -> Term
app f x = App "$ap" [f,x]

pp, qq :: Term -> Term
pp x = App "$p" [x]
qq x = App "$q" [x]

isType h = h `elem` ["man", "woman", "donkey"]

(-:) :: Term -> TT -> FO
p -: Pi    x a b = All x ((Var x -: a) :=>: (app p (Var x) -: b))
p -: Sigma x a b = (pp p -: a) :&: (qq p -: subst [(x,pp p)] b)
p -: Basic h ts
  | isType h     = Atom h (ts ++ [p])
  | otherwise    = Atom h ts

class Subst a where
  free  :: a -> [Name]
  names :: a -> [Name]
  subst :: [(Name,Term)] -> a -> a

instance Subst () where
  free _    = []
  names _   = []
  subst _ _ = ()

instance (Subst a, Subst b) => Subst (a,b) where
  free (x,y)      = free x `union` free y
  names (x,y)     = names x `union` names y
  subst sub (x,y) = (subst sub x, subst sub y)

instance Subst a => Subst [a] where
  free xs      = foldr union [] (map free xs)
  names xs     = foldr union [] (map names xs)
  subst sub xs = map (subst sub) xs

instance Subst Term where
  free (Var x)    = [x]
  free (App _ xs) = free xs

  names (Var x)    = [x]
  names (App f xs) = [f] `union` names xs

  subst sub (Var x)    = head ([ t | (y,t) <- sub, y == x ] ++ [Var x])
  subst sub (App f xs) = App f (subst sub xs)

data Bind a = Bind Name a

fresh :: [Name] -> [Name]
fresh nots = [ "x" ++ show i | i <- [0..] ] \\ nots

instance Subst a => Subst (Bind a) where
  free  (Bind x t) = free t \\ [x]
  names (Bind x t) = names t `union` [x]

  subst sub (Bind x t)
    | x `elem` nots = Bind x' (subst ((x,Var x'):sub') t)
    | otherwise     = Bind x  (subst sub' t)
   where
    sub' = filter ((x/=).fst) sub
    nots = free (map snd sub')
    x':_ = fresh nots

instance Subst TT where
  free (Pi    x a b) = free (a, Bind x b)
  free (Sigma x a b) = free (a, Bind x b)
  free (Basic _ xs)  = free xs
  
  names (Pi    x a b) = names (a, Bind x b)
  names (Sigma x a b) = names (a, Bind x b)
  names (Basic _ xs)  = names xs
  
  subst sub (Pi    x a b) = let (a',Bind x' b') = subst sub (a,Bind x b) in Pi x' a' b'
  subst sub (Sigma x a b) = let (a',Bind x' b') = subst sub (a,Bind x b) in Sigma x' a' b'
  subst sub (Basic h xs)  = Basic h (subst sub xs)

instance Subst FO where
  free (All x p)   = free (Bind x p)
  free (Exi x p)   = free (Bind x p)
  free (p :&: q)   = free (p,q)
  free (p :=>: q)  = free (p,q)
  free (Atom _ xs) = free xs

  names (All x p)   = names (Bind x p)
  names (Exi x p)   = names (Bind x p)
  names (p :&: q)   = names (p,q)
  names (p :=>: q)  = names (p,q)
  names (Atom _ xs) = names xs

  subst sub (All x p)   = let Bind x' p' = subst sub (Bind x p) in All x' p'
  subst sub (Exi x p)   = let Bind x' p' = subst sub (Bind x p) in Exi x' p'
  subst sub (p :&: q)   = subst sub p :&: subst sub q
  subst sub (p :=>: q)  = subst sub p :=>: subst sub q
  subst sub (Atom h xs) = Atom h (subst sub xs)

--------------------------------------------------------------------------------

removePairs :: FO -> FO
removePairs (All x p)
  | isPair x p          = removePairs (removePairAll x p)
removePairs (All x p)   = All x (removePairs p)
removePairs (Exi x p)   = Exi x (removePairs p)
removePairs (p :&: q)   = removePairs p :&:  removePairs q
removePairs (p :=>: q)  = removePairs p :=>: removePairs q
removePairs (Atom h xs) = Atom h xs

isPair :: Name -> FO -> Bool
isPair x (All y p)   = x == y || isPair x p
isPair x (Exi y p)   = x == y || isPair x p
isPair x (p :&: q)   = isPair x p && isPair x q
isPair x (p :=>: q)  = isPair x p && isPair x q
isPair x (Atom h xs) = all (isPairTerm x) xs

isPairTerm :: Name -> Term -> Bool
isPairTerm x (App f [t])
  | f `elem` ["$p","$q"] && t == Var x = True
isPairTerm x (App f ts) = all (isPairTerm x) ts
isPairTerm x (Var y)    = x /= y

removePairAll :: Name -> FO -> FO
removePairAll x p
  | x `elem` free p = All x0 (All x1 (rem p))
  | otherwise       = p
 where
  x0:x1:_ = fresh (names p)
  
  rem (All y p)   = All y (rem p)
  rem (Exi y p)   = Exi y (rem p)
  rem (p :&: q)   = rem p :&: rem q
  rem (p :=>: q)  = rem p :=>: rem q
  rem (Atom h ts) = Atom h (map remTerm ts)
  
  remTerm (App f [t])
    | f == "$p" && t == Var x = Var x0
    | f == "$q" && t == Var x = Var x1
  remTerm (App f ts)          = App f (map remTerm ts)
  remTerm (Var x)             = Var x

--------------------------------------------------------------------------------

aManHasADonkey = Sigma "x"
                   (Basic "man" [])
                   (Sigma "y"
                     (Basic "donkey" [])
                     (Basic "has" [Var "x", Var "y"]))

heBeatsIt p = Basic "beats" [pp p, pp (qq p)]

ifAManHasADonkeyHeBeatsIt = Pi "c" aManHasADonkey (heBeatsIt (Var "c"))

