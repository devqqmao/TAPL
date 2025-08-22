infixl 4 :@:

data Term = Idx Int
          | Term :@: Term
          | Lmb Term
          deriving (Eq, Read, Show)

go :: Int -> Int -> Term -> Term
go m k (Idx n) = if n < m then Idx n else Idx $ n + k
go m k (t0 :@: t1) = (go m k t0) :@: (go m k t1)
go m k (Lmb t0) = Lmb (go (m + 1) k t0)

shift :: Int -> Term -> Term
shift val term = go 0 val term

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx n) = if n == j then s else Idx n
substDB j s (t0 :@: t1) = (substDB j s t0) :@: (substDB j s t1)
substDB j s (Lmb t) = Lmb (substDB (j+1) (shift 1 s) t)

betaRuleDB :: Term -> Term
betaRuleDB (Lmb t :@: s) = shift (-1) (substDB 0 (shift 1 s) t)


-- 1.1 if Idx -> Nothing
-- 1.2 if Lam -> reduce it's argument <$>
-- 1.3 if App ->
-- 2.1 try to reduce left
-- 2.2 if unsucessful if left is lambda -> apply
-- 2.3 if not lambda -> try to reduce right <$>
oneStepDBN :: Term -> Maybe Term
oneStepDBN t = case t of
    Idx _ -> Nothing
    Lmb t -> Lmb <$> oneStepDBN t
    t0 :@: t1 -> case oneStepDBN t0 of
        Just t00 -> Just $ t00 :@: t1
        Nothing -> case t0 of
            Lmb t01 -> Just $ betaRuleDB t
            _ -> ((:@:) t0) <$> oneStepDBN t1

oneStepDBA :: Term -> Maybe Term
oneStepDBA t = case t of
    Idx _ -> Nothing
    Lmb t -> Lmb <$> oneStepDBA t
    t0 :@: t1 -> case oneStepDBA t1 of
        Just t2 -> Just $ t0 :@: t2
        Nothing -> case t0 of
            Lmb _ -> Just $ betaRuleDB t
            _ -> case oneStepDBA t0 of
                Nothing -> Nothing
                Just t3 -> Just $ t3 :@: t1

-- идея: просто бесконечно применять стратегию, пока не даст Nothing
nfDB :: (Term -> Maybe Term) -> Term -> Term
nfDB f t = case f t of
    Nothing -> t
    Just t0 -> nfDB f t0

nfDBN = nfDB oneStepDBN
nfDBA = nfDB oneStepDBA
cIDB = Lmb (Idx 0)
cKDB = Lmb (Lmb (Idx 1))
comegaDB = Lmb (Idx 0 :@: Idx 0)
cOmegaDB = comegaDB :@: comegaDB
--nfDBN (cKDB :@: cIDB :@: cOmegaDB)
--r0 = Lmb (Idx 0)
--r1 = nfDBA (cKDB :@: cIDB :@: cOmegaDB)
