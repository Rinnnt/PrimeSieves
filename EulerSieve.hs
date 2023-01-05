import Data.List

primesTo m = 2 : eulers [3,5..m]
  where
    eulers (p : xs) = p : eulers (xs \\ map (p *) (p : xs))
