import Control.Applicative

type Symb = String
infixl 4 :@: 
infixr 3 :->

data Type = Boo
          | Type :-> Type
    deriving (Read,Show,Eq)

data Term = Fls
          | Tru
          | If Term Term Term
          | Idx Int
          | Term :@: Term
          | Lmb Symb Type Term
          deriving (Read,Show)

instance Eq Term where
  Fls       == Fls         =  True
  Tru       == Tru         =  True
  If b u w  == If b1 u1 w1 =  b == b1 && u == u1 && w == w1
  Idx m     == Idx m1      =  m == m1
  (u:@:w)   == (u1:@:w1)   =  u == u1 && w == w1
  Lmb _ t u == Lmb _ t1 u1 =  t == t1 && u == u1
  _         == _           =  False

newtype Env = Env [(Symb,Type)]
  deriving (Read,Show,Eq)


lift :: Int -> Int -> Term -> Term
lift m k (Idx n) | n >= m = Idx (n + k)
lift m k u@(Idx n)        = u
lift m k (p :@: q)        = (lift m k p) :@: (lift m k q)
lift m k (Lmb n t p)      = Lmb n t $ lift (m+1) k p
lift m k Tru              = Tru
lift m k Fls              = Fls
lift m k (If b u w)       = If (lift m k b) (lift m k u) (lift m k w)

shift :: Int -> Term -> Term
shift = lift 0

liftOnce :: Term -> Term
liftOnce = shift 1

unlift :: Term -> Term
unlift = shift (-1)

subst :: Int -> Term -> Term -> Term
subst j t (Idx k) | k == j = t
subst j t u@(Idx k)        = u
subst j t (p :@: q)        = (subst j t p) :@: (subst j t q)
subst j t u@(Lmb n tp p)   = Lmb n tp (subst (j + 1) (liftOnce t) p)
subst j t Tru              = Tru
subst j t Fls              = Fls
subst j t (If b u w)       = If (subst j t b) (subst j t u) (subst j t w)

substDB = subst
isValue :: Term -> Bool
isValue Fls = True
isValue Tru = True
isValue (Lmb _ _ _) = True
isValue _ = False

isIrreducible :: Term -> Bool
isIrreducible (Idx _) = True
isIrreducible t = isValue t

betaRuleDB :: Term -> Term
betaRuleDB (Lmb c t m :@: n) = unlift $ subst 0 (liftOnce n) m
betaRuleDB _ = undefined

oneStepDBA :: Term -> Maybe Term
oneStepDBA t | isIrreducible t = Nothing
oneStepDBA u@(Lmb c t m :@: n) | isIrreducible n = Just (betaRuleDB u)
oneStepDBA (Lmb c t m :@: n) = ((Lmb c t m :@:) <$> oneStepDBA n)
oneStepDBA (a :@: b) = ((:@: b) <$> oneStepDBA a)
oneStepDBA (If Fls x y) = Just y
oneStepDBA (If Tru x y) = Just x
oneStepDBA (If x y z) = (\x' -> If x' y z) <$> oneStepDBA x

oneStep = oneStepDBA

nfDB :: (a -> Maybe a) -> a -> a
nfDB f t = case f t of
  Just x  -> nfDB f x
  Nothing -> t

whnf :: Term -> Term 
whnf = nfDB oneStep

-- взял у Саши Кудрявцева, т.к. я не смог решить, а дальше это надо.