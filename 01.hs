isPerfect n = n == sum [i | i <- [1 .. n-1], n `mod` i == 0]

perfectNumbers :: Integral a => [a]
perfectNumbers = filter isPerfect [1..]
