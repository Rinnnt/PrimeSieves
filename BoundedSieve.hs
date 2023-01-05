import Data.List

minus :: [Int] -> [Int] -> [Int]
minus (x : xs) (y : ys)
  | x < y  = x : minus xs (y : ys)
  | x == y =     minus xs ys
  | x > y  =     minus (x : xs) ys
minus xs _ = xs

sqrtInt :: Int -> Int
sqrtInt = floor . sqrt . fromIntegral

primesTo m = 2 : sieve [3,5..m]
  where
    sqrtM = sqrtInt m
    sieve [] = []
    sieve (p : xs)
      | p > sqrtM = (p : xs)
      | otherwise =  p : sieve (xs `minus` takeWhile (<= m) [p*p,p*p+2*p..])

main :: IO ()
main = print (last $ primesTo 5000000)

