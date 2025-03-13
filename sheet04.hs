-- A1
data Peano
  = Zero
  | Succ Peano

-- a
instance Eq Peano where
  (==) Zero Zero = True
  (==) (Succ a) (Succ b) = a == b
  (==) _ _ = False

-- b
instance Ord Peano where
  (<=) Zero _ = True
  (<=) (Succ _) Zero = False
  (<=) (Succ a) (Succ b) = a <= b

-- c
-- pred Zero ist nicht definiert, da es keinen Vorgänger von Zero gibt -> Fehler muss geworfen werden
instance Enum Peano where
  toEnum n
    | n < 0 = error "Peano numbers can't be negative!"
    | n == 0 = Zero
    | otherwise = Succ (toEnum (n - 1))
  fromEnum Zero = 0
  fromEnum (Succ a) = 1 + fromEnum a
  pred Zero = error "Peano numbers can't have a predecessor of Zero!"
  pred (Succ a) = a

instance Show Peano where
  show Zero = "Zero"
  show (Succ a) = "Succ " ++ show a

-- d
instance Num Peano where
  (+) a b = toEnum (fromEnum a + fromEnum b)
  (*) a b = toEnum (fromEnum a * fromEnum b)
  (-) a b
    | a < b = error "Peano numbers can't be negative!"
    | a == b = Zero
    | otherwise = toEnum (fromEnum a - fromEnum b)
  abs a = a
  signum a = if a == Zero then Zero else Succ Zero
  fromInteger n = toEnum (fromIntegral n)
  negate _ = error "Peano numbers can't be negative!"

-- e
instance Real Peano where
  toRational a = toRational (fromEnum a)

-- f
instance Integral Peano where
  quotRem a b
    | b == Zero = error "Division by Zero!"
    | a < b = (Zero, a)
    | otherwise = let (q, r) = quotRem (a - b) b in (Succ q, r)
  toInteger a = fromIntegral (fromEnum a)

-- g
fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

-- A2
-- a
instance (Num a) => Num (Maybe a) where
  (+) (Just x) (Just y) = Just (x + y)
  (+) _ _ = Nothing
  (*) (Just x) (Just y) = Just (x * y)
  (*) _ _ = Nothing
  abs Nothing = Nothing
  abs (Just x) = Just (abs x)
  signum Nothing = Nothing
  signum (Just x) = Just (signum x)
  fromInteger _ = Nothing
  negate Nothing = Nothing
  negate (Just x) = Just (negate x)
  (-) (Just x) (Just y) = Just (x - y)
  (-) _ _ = Nothing
-- b
instance (Num a, Num b, Num c) => Num (a, b, c) where
  (+) (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  (*) (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger _ = (0, 0, 0)
  negate (x, y, z) = (negate x, negate y, negate z)
  (-) (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
