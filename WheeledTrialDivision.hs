sqrtInt :: Int -> Int
sqrtInt = floor . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime n = all ((/= 0) . (n `mod`)) facts
  where
    facts = takeWhile (<= sqrtInt n) primes

primes = 2 : 3 : 5 : 7 : [x | x <- wheeled, isPrime x]

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

wheeled = scanl (+) 11 wheel

main :: IO ()
main = print (last $ takeWhile (<= 5000000) primes)

