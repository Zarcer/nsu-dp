module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where


unique :: Eq a => [a] -> Bool
unique [] = True
unique (x:xs)
  | x `elem` xs = False
  | otherwise = unique xs

pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(x,y,z) |  z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2]

primitivePythagoreanTriples :: Integral a => [(a,a,a)]
primitivePythagoreanTriples = [(x,y,z) | z<-[1..], y<-[1..(z-1)], x<-[1..(y-1)], x*x+y*y==z*z, gcd x y == 1]

isPerfect n = n == sum [i | i <- [1 .. n-1], n `mod` i == 0]
perfectNumbers :: Integral a => [a]
perfectNumbers = filter isPerfect [1..]

cantorPairs :: Integral a => [(a, a)]
cantorPairs = undefined


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
