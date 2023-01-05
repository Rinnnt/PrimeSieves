sqrtInt :: Int -> Int
sqrtInt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = all ((/= 0) . (n `mod`)) facts
  where
    facts = takeWhile (<= sqrtInt n) primes

primes = 2 : [x | x <- [3..], isPrime x]

main :: IO ()
main = print (last $ takeWhile (<= 5000000) primes)

