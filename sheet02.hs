-- A1
-- a
twice :: (a -> a) -> a -> a
twice f x = f (f x)
-- b
chain :: (b -> c) -> (a -> b) -> a -> c
chain f g x = f (g x)
-- c
flop :: (a -> b -> c) -> b -> a -> c
flop f x y = f y x
-- A2
-- a
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs
-- b
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]
-- c
divisors :: Integer -> [Integer]
divisors n
  | n < 3     = []
  | otherwise = help 2 n
  where
    help i n
      | i == n = []
      | n `mod` i == 0 = i : help (i+1) n
      | otherwise = help (i+1) n
-- d
mersennes :: [Integer]
mersennes = [2^p - 1 | p <- [2..]]
-- e
primes :: [Integer]
primes = [p | p <- [2..], null (divisors p)]
-- f
pyths :: [(Integer, Integer, Integer)]
pyths = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], x^2 + y^2 == z^2, gcd x (gcd y z) == 1]
-- A3
-- a
primeTwins :: [(Integer, Integer)]
primeTwins = filter (\(x, y) -> y - x == 2) [(p, q) | (p, q) <- zip primes (tail primes)]
-- b
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []
-- c
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p l = foldr (\x xs -> if p x then x : xs else []) [] l
-- d
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' l = foldr (++) [] l