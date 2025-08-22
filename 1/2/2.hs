import Data.List (lookup)
import Data.Maybe (fromJust)

type Symb = String
infixl 4 :@:
infixr 3 :->

data Type = Boo
          | Type :-> Type
    deriving (Read, Show, Eq)

data Term = Fls
          | Tru
          | If Term Term Term
          | Idx Int
          | Term :@: Term
          | Lmb Symb Type Term
          deriving (Read, Show)

instance Eq Term where
  Fls       == Fls         = True
  Tru       == Tru         = True
  If b u w  == If b1 u1 w1 = b == b1 && u == u1 && w == w1
  Idx m     == Idx m1      = m == m1
  (u:@:w)   == (u1:@:w1)   = u == u1 && w == w1
  Lmb _ t u == Lmb _ t1 u1 = t == t1 && u == u1
  _         == _           = False

newtype Env = Env [(Symb, Type)]
  deriving (Read, Show, Eq)

infer :: Env -> Term -> Maybe Type
infer (Env env) term = case term of
  Fls -> Just Boo
  Tru -> Just Boo
  If b t e -> do
    bType <- infer (Env env) b
    tType <- infer (Env env) t
    eType <- infer (Env env) e
    if bType == Boo && tType == eType
      then Just tType
      else Nothing

  Idx n -> if n >= 0 && n < length env then Just (snd (env !! n)) else Nothing

  Lmb sym typ body -> do
    let newEnv = Env $ (sym, typ) : env
    bodyType <- infer newEnv body
    return (typ :-> bodyType)

  t1 :@: t2 -> do
    t1Type <- infer (Env env) t1
    t2Type <- infer (Env env) t2
    case t1Type of
      (argType :-> resType) | argType == t2Type -> Just resType
      _ -> Nothing

infer0 :: Term -> Maybe Type
infer0 = infer (Env [])

cKB = Lmb "x" Boo (Lmb "y" Boo (Idx 1))
r0 = infer0 cKB
-- (Boo :-> (Boo :-> Boo))
r1 = infer0 (cKB :@: Tru)
-- (Boo :-> Boo)
r2 = infer0 (cKB :@: Tru :@: If Tru Fls Tru)
-- Boo
env = Env[("x",Boo),("y",Boo :-> Boo)]
term = Idx 1 :@: Idx 0
r3 = infer env term
-- Boo
--term = Idx 0 :@: Idx 1
--r4 = infer env term
-- Nothing