import Data.List (elemIndex)

type Symb = String

infixl 4 :@:
infixl 4 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

data Term = Idx Int
          | Term :@: Term
          | Lmb Symb Term
          deriving (Read, Show)

instance Eq Term where
  Idx m     == Idx n      =  m == n
  (t1:@:s1) == (t2:@:s2)  =  t1 == t2 && s1 == s2
  Lmb _ t1  == Lmb _ t2   =  t1 == t2
  _         == _          =  False

type Context = [Symb]

e2t :: Expr -> (Context, Term)
e2t expr = (ctx, term)
  where
    (term, ctx) = convert expr [] []

    convert :: Expr -> [Symb] -> [Symb] -> (Term, [Symb])
    convert (Var x) currentBindings globalCtx =
        case elemIndex x currentBindings of
            Just idx -> (Idx idx, globalCtx)
            Nothing ->
                case elemIndex x globalCtx of
                    Just idx -> (Idx (length currentBindings + idx), globalCtx)
                    Nothing ->
                        let newGlobal = globalCtx ++ [x]
                        in (Idx (length currentBindings + length globalCtx), newGlobal)

    convert (e1 :@ e2) currentBindings globalCtx =
        let (t1, gc1) = convert e1 currentBindings globalCtx
            (t2, gc2) = convert e2 currentBindings gc1
        in (t1 :@: t2, gc2)

    convert (Lam x e) currentBindings globalCtx =
        let (t, gc') = convert e (x : currentBindings) globalCtx
        in (Lmb x t, gc')


t2e :: Context -> Term -> Expr
t2e ctx term = helper term []
  where
    helper :: Term -> [Symb] -> Expr
    helper (Idx n) binders
        | n < length binders = Var (binders !! n)
        | otherwise =
            let freeIndex = n - length binders
            in Var (ctx !! freeIndex)
    helper (t1 :@: t2) binders = helper t1 binders :@ helper t2 binders
    helper (Lmb sym body) binders =
        let freshName = fresh sym binders
        in Lam freshName (helper body (freshName : binders))

    fresh :: Symb -> [Symb] -> Symb
    fresh name binders =
        head $ filter (not . (`elem` binders)) $ iterate addPrime name

    addPrime s = s ++ "'"