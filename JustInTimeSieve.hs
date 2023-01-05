type PrimePair = (Int, Int)
type Postponed = [(Int, Int)]

update :: Int -> Postponed -> Postponed
update a ps
  = map update' ps
  where
    update' :: PrimePair -> PrimePair
    update' u@(m, p)
      | a == m    = (m + 2 * p, p)
      | otherwise = u

primes = 2 : sieve [3,5..] []
  where
    sieve :: [Int] -> Postponed -> [Int]
    sieve (p : xs) ps
      | p `elem` (map fst ps) = sieve xs (update p ps)
      | otherwise             = p : sieve xs ((p * p, p) : ps)
