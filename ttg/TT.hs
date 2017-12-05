module TT where

import Data.List( union, (\\), intercalate )
import Data.Char( isUpper )

--------------------------------------------------------------------------------

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
  | Or TT TT
  | Equals Term Term
  | Bottom
 deriving ( Eq, Ord )
 
instance Show TT where
  show (Pi    x a b) | not (x `elem` free b) = "(" ++ show a ++ " ==> " ++ show b ++ ")"
                     | otherwise =  "(Π" ++ x ++ ":" ++ show a ++ ")" ++ show b
  show (Sigma x a b) | not (x `elem` free b)  = "(" ++ show a ++ " & " ++ show b ++ ")"
                     | otherwise = "(Σ" ++ x ++ ":" ++ show a ++ ")" ++ show b
  show (Basic h [])  = h
  show (Basic h xs)  = h ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show (Equals t1 t2) = show t1 ++ " = " ++ show t2 
  show (Or a b) = "(" ++ show a ++ " + " ++  show b ++ ")"
  show Bottom = "⊥"

data FO
  = All Name FO
  | Exi Name FO
  | FO :&: FO
  | FO :=>: FO
  | FO :||: FO
  | Term :=: Term
  | Atom Name [Term]
  | PTrue
  | PFalse
 deriving ( Eq, Ord )

instance Show FO where
  show (All x p)   = "∀" ++ x ++ "." ++ show p
  show (Exi x p)   = "∃" ++ x ++ "." ++ show p
  show (p :&: q)   = "(" ++ show p ++ " & " ++ show q ++ ")"
  show (p :=>: q)  = "(" ++ show p ++ " => " ++ show q ++ ")"
  show (p :||: q)  = "(" ++ show p ++ " | " ++ show q ++ ")"
  show (t1 :=: t2) = "(" ++ show t1 ++ " = " ++ show t2 ++ ")"
  show (Atom h []) = h
  show (Atom h xs) = h ++ "(" ++ intercalate "," (map show xs) ++ ")"
  show PFalse = "false"
  show PTrue  = "true"

--------------------------------------------------------------------------------

fo :: TT -> FO
fo (Pi    x a b) = All x (proof x a b :=>: fo b)
fo (Sigma x a b) = Exi x (proof x a b :&:  fo b)
fo (Equals t1 t2) = t1 :=: t2
fo (Basic h ts)  = Atom h ts
fo (Or a b) = fo a :||: fo b 
fo Bottom = PFalse


proof :: Name -> TT -> TT -> FO
proof x a b
  | x `elem` free b || needsProof a = Var x -: a
  | otherwise                       = fo a
-- where

needsProof (Pi    _ _ b) = needsProof b
needsProof (Sigma _ _ b) = needsProof b
needsProof (Basic h _)   = isType h
needsProof (Or a b) = needsProof a || needsProof b
needsProof _ = False


(-:) :: Term -> TT -> FO
--p -: Pi    x a b = error ("higher-order TT (" ++ show p ++ " : " ++ show (Pi x a b) ++ ")")
p -: Pi    x a b = All x ((Var x -: a) :=>: (ap p (Var x) -: b))
p -: Sigma x a b = (pp p -: a) :&: (qq p -: subst [(x,pp p)] b)
p -: Basic h ts
  | isType h     = Atom h (ts ++ [p])
  | otherwise    = Atom h ts
p -: Equals t1 t2 = t1 :=: t2 
p -: Or a b  = (p -: a) :||: (p -: b)
p -: Bottom = PFalse

ap :: Term -> Term -> Term
ap f x = App "$ap" [f,x]

pp, qq :: Term -> Term
pp x = App "$p" [x]
qq x = App "$q" [x]

isType h = all isUpper (take 1 h)

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
  free (Equals a b) = free (a,b)
  free (Or a b) = free (a,b)
  free Bottom = []
  
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

conjecture :: TT -> FO
conjecture tt = removePairs (fo tt)

removePairs :: FO -> FO
removePairs (All x p)
  | isPair x p          = removePairs (removePair All x p)
removePairs (Exi x p)
  | isPair x p          = removePairs (removePair Exi x p)
removePairs (All x p)   = All x (removePairs p)
removePairs (Exi x p)   = Exi x (removePairs p)
removePairs (p :&: q)   = removePairs p :&:  removePairs q
removePairs (p :=>: q)  = removePairs p :=>: removePairs q
removePairs (Atom h xs) = Atom h xs
removePairs PTrue = PTrue
removePairs PFalse = PFalse

isPair :: Name -> FO -> Bool
isPair x (All y p)   = x == y || isPair x p
isPair x (Exi y p)   = x == y || isPair x p
isPair x (p :&: q)   = isPair x p && isPair x q
isPair x (p :=>: q)  = isPair x p && isPair x q
isPair x (Atom h xs) = all (isPairTerm x) xs
isPair _ PFalse = False
isPair _ PTrue = False


isPairTerm :: Name -> Term -> Bool
isPairTerm x (App f [t])
  | f `elem` ["$p","$q"] && t == Var x = True
isPairTerm x (App f ts) = all (isPairTerm x) ts
isPairTerm x (Var y)    = x /= y

removePair :: (Name -> FO -> FO) -> Name -> FO -> FO
removePair quant x p
  | x `elem` free p = quant x0 (quant x1 (rem p))
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

notTT a    = Pi "notTT" a Bottom
andTT a b  = Sigma "andTT" a b
orTT a b   = notTT (andTT (notTT a) (notTT b))
implTT a b = orTT (notTT a) b

man        = Basic "Man" []
rich x     = Basic "rich" [x]
donkey     = Basic "Donkey" []
have  x y  = Basic "has" [x,y]
get x y    = Basic "get" [x,y]
beats x y  = Basic "beats" [x,y]
opens x y  = Basic "opens" [x,y]
equals x y = Equals x y

child      = Basic "Child" []
present    = Basic "Present" []    
    
constf x       = App "const" [x]
override x y z = App "override" [x,y,z]


piano      = App "piano" []
pyssel     = App "pyssel" []
vincent    = App "vincent" []
irmeli     = App "irmeli" []


aManHasADonkey = Sigma "x"
                   man
                   (Sigma "y"
                     donkey
                     (Var "x" `have` Var "y"))
                     
-- if all children get presents, there is a child that opens theirs
ifAllGetPresentsSomeoneOpensTheirs = Pi "f" (Pi "x" child (Var "x" `get` (ap (Var "f") (Var "x")))) 
         (Sigma "y" child (Var "y" `opens` (ap (Var "f") (Var "y"))))               
                     
allChildrenGetPresents = Pi "x" child
                           (Sigma "y" present (Var "x" `get` Var "y"))
                           
-- A child is either Vincent or Irmeli                           
onlyChildren = Pi "x" child (Or (equals (Var "x") vincent) ((equals (Var "x") irmeli)))

-- A present is either piano or pyssel
onlyPresents = Pi "x" present (Or (equals (Var "x") piano) ((equals (Var "x") pyssel)))

--Vincent does not open any present
vincentDoesntOpen = Pi "x" present (notTT (vincent `opens` (Var "x")))

-- Irmeli doesn't get a piano
irmeliDoesntGetPiano = notTT (irmeli `get` piano) 

irmeliOpensPyssel = (irmeli `opens` pyssel)

vincentIsAChild = vincent -: child 
irmeliIsAChild  = irmeli -: child

pysselIsAPresent = pyssel -: present
pianoIsAPresent  = piano -: present

heBeatsIt p = pp p `beats` pp (qq p)

ifAManHasADonkeyHeBeatsIt = Pi "c" aManHasADonkey (heBeatsIt (Var "c"))
        
allMenHaveADonkey = Pi "x" man (Sigma "y" donkey (Var "x" `have` Var "y"))

higherOrder = Pi "x" man (Pi "_r" (Pi "y" donkey (Var "x" `have` Var "y")) (rich (Var "x"))) 

karttunen = Pi "f" (Pi "x" man (Sigma "y" donkey (have (Var "x") (Var "y"))))
              (Sigma "u" man (beats (Var "u") (pp (ap (Var "f") (Var "u")))))

everyCustomerPays =
  Pi "x" customer (pay (Var "x"))

customer = Basic "Customer" []
company  = Basic "Company" []
pay x    = Basic "pay" [x]

facebook = App "facebook" []

facebookIsACustomer =
  facebook -: customer

facebookIsACompany =
  facebook -: company

question =
  pay facebook

-- main = writeTPTP "q.p" ((facebookIsACompany :&: facebookIsACustomer :&: conjecture everyCustomerPays) :=>: conjecture question)

main2 = writeTPTP "q.p" 
   [
    fo onlyPresents, fo onlyChildren, vincentIsAChild, irmeliIsAChild, pysselIsAPresent, pianoIsAPresent,
    fo ifAllGetPresentsSomeoneOpensTheirs, fo allChildrenGetPresents, fo vincentDoesntOpen, fo irmeliDoesntGetPiano  ]
    (conjecture irmeliOpensPyssel)

{-

EXI p . ALL x . man(x) => ( donkey(fst(p@x)) & has(x,fst(p@x)) )

--> { because p : _ -> (_,_) }

EXI p0,p1 . ALL x . man(x) => ( donkey(p0@x) & has(x,p0@x) )

--> { because p1 is not used }

EXI p0 . ALL x . man(x) => ( donkey(p0@x) & has(x,p0@x) )

--> { p0 becomes a top-level skolem function }

ALL x . man(x) => ( donkey(sk(x)) & has(x,sk(x)) )

-}

{-

EXI p . ALL f .
    (ALL x . (man(x) => has(x,fst(f@x))))
  =>
    (ALL y . (man(y) => has(x,fst((p@f)@y))))

-->

EXI p . ALL f .
    (ALL x . (man(x) => has(x,fst(f@x))))
  =>
    (ALL y . (man(y) => has(x,fst(p@(f,y)))))

-->

EXI p . ALL f0, f1 .
    (ALL x . (man(x) => has(x,f0@x))))
  =>
    (ALL y . (man(y) => has(x,fst(p@(f0,f1,y)))))

-->

EXI p0 . ALL f0, f1 .
    (ALL x . (man(x) => has(x,f0@x))))
  =>
    (ALL y . (man(y) => has(x,p0@(f0,f1,y))))

-}

--------------------------------------------------------------------------------

{-
data Type
  = UNIT
  | TERM
  | Type :->: Type
  | Type :*: Type
 deriving ( Eq, Ord, Show )

data Object
  = U
  | T Term
  | F (Object -> Object)
  | P Object Object

app :: Object -> Object -> Object
F f `app` x = f x

pp, qq :: Object -> Object
pp (x :*: _) = x
qq (_ :*: y) = y

quant :: (Term -> Form) -> Type -> [Name] -> (Object -> Form) -> Form
quant qu t p =
  -}

--------------------------------------------------------------------------------

writeTPTP :: FilePath -> [FO] -> FO -> IO ()
writeTPTP file ps p = writeFile file $ unlines $
  ["fof(h,axiom, ![X,Y] : (sp_ap(const(Y),X) = Y)).",
   "fof(i,axiom, ![F,X,Y] : sp_ap(override(F,X,Y),X) = Y).",
   "fof(j,axiom, ![F,X,Y,Z] : (Z != X => (sp_ap(override(F,X,Y),Z) = sp_ap(F,Z))))."
  ] ++ 
  [   
    "fof( a ,axiom, " ++ pp p' ++  " )." | p' <- ps] ++ 
    
  [ "fof(conj, conjecture, "
  , "  " ++ pp p
  , ")."
  ]
 where
   pp = prTPTP

prTPTP :: FO -> String
prTPTP = pp where
  pp (All x p)   = "(![" ++ "V" ++ nm x ++ "]: " ++ pp p ++ ")"
  pp (Exi x p)   = "(?[" ++ "V" ++ nm x ++ "]: " ++ pp p ++ ")"
  pp (p :&: q)   = "(" ++ pp p ++ " & " ++ pp q ++ ")"
  pp (p :=>: q)  = "(" ++ pp p ++ " => " ++ pp q ++ ")"
  pp (Atom h ts) = pt (App ("p"++h) ts)
  pp (t1 :=: t2) = pt t1 ++ " = " ++ pt t2
  pp (p :||: q) = "(" ++ pp p ++ " | " ++ pp q ++ ")"
  pp PFalse = "$false"
  pp PTrue = "$true"
  
  pt (App f []) = nm f
  pt (App f ts) = nm f ++ "(" ++ intercalate "," (map pt ts) ++ ")"
  pt (Var x)    = "V" ++ nm x
  
  nm = concatMap (\c -> if c == '$' then "sp_" else [c])
  