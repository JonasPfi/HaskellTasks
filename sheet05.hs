import Text.Read (readMaybe)

--- 1 ---

data Person = Person {
        name   :: String,
        age    :: Int,
        emails :: String
    } deriving Show

type Record = [(String,String)]

lookupPerson :: Record -> Maybe Person
lookupPerson rec =
    case lookup "name" rec of
        Nothing     -> Nothing
        Just name   -> case lookup "age" rec of
            Nothing     -> Nothing
            Just age    -> case lookup "email" rec of
                Nothing -> Nothing
                Just email -> Just $ Person name (read age) email

steve = [("name", "Steve"), ("age", "42"), ("email", "steve69@hotmail.com")]

-- a
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup sucht in einer Liste von Paaren nach einem bestimmten Schlüssel

-- read :: Read a => String -> a
-- read wandelt einen String in einen Wert eines bestimmten Typs um

-- b
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' key [] = Nothing
lookup' key ((k,v):xs)
    | key == k  = Just v
    | otherwise = lookup' key xs

-- c
lookupPerson' :: Record -> Maybe Person
lookupPerson' rec =
    fmap Person (lookup "name" rec)
    <*> (lookup "age" rec >>= readMaybe)
    <*> lookup "email" rec

-- d
lookupPerson'' :: Record -> Maybe Person
lookupPerson'' rec = do
    name <- lookup "name" rec
    age <- lookup "age" rec >>= readMaybe
    email <- lookup "email" rec
    return (Person name age email)

-- e
lookupPersonSafe :: Record -> Either String Person
lookupPersonSafe rec = do
    name <- maybe (Left "name not found") Right (lookup "name" rec)
    ageStr <- maybe (Left "age not found") Right (lookup "age" rec)
    age <- maybe (Left "age is not an Int") Right (readMaybe ageStr)
    email <- maybe (Left "email not found") Right (lookup "email" rec)
    return (Person name age email)


--- 2 ---

data Treasure = Diamonds deriving Show
data Trap     = Trap deriving (Show, Eq)

type Length = Int

data Maze     = Straight (Maybe Trap) Length Maze
              | TCross Maze Maze
              | End (Maybe (Either Trap Treasure))
    deriving Show

aTrap = Just Trap
deadEnd = End Nothing
trapEnd = End (Just (Left Trap))
diamEnd = End (Just (Right Diamonds))


bspMaze = Straight Nothing 100 (TCross
        trapEnd
        (TCross
            (Straight Nothing 200 (TCross
                (Straight Nothing 50 (TCross
                    (Straight aTrap 50 deadEnd)
                    diamEnd
                ))
                trapEnd
            ))
            deadEnd
        ))

-- a
totalDist :: Maze -> Int
totalDist (End _) = 0
totalDist (Straight _ l m) = l + totalDist m
totalDist (TCross m1 m2) = 150 + totalDist m1 + totalDist m2

-- b
foldMaze :: (Maybe Trap -> Length -> t -> t) -> (t -> t -> t) -> (Maybe (Either Trap Treasure) -> t) -> Maze -> t
foldMaze f1 f2 f3 = fold where
    fold (Straight t l m) = f1 t l (fold m)
    fold (TCross m1 m2) = f2 (fold m1) (fold m2)
    fold (End x) = f3 x

-- c
countTraps :: Maze -> Int
countTraps = foldMaze f1 f2 f3
  where
    f1 (Just _) _ acc = acc + 1  
    f1 Nothing _ acc  = acc      
    f2 acc1 acc2      = acc1 + acc2  
    f3 (Just (Left _)) = 1        
    f3 _              = 0       

-- d 
routeToDiamonds :: Maze -> Maybe [String]
routeToDiamonds maze = fmap snd (foldMaze f1 f2 f3 maze)
  where
    f1 _ l (Just (dist, path)) = Just (dist + l, "S" : path)
    f1 _ _ Nothing             = Nothing

    f2 (Just (d1, path1)) (Just (d2, path2))
        | d1 <= d2  = Just (d1 + 150, "L" : path1)
        | otherwise = Just (d2 + 150, "R" : path2)
    f2 (Just (d1, path1)) Nothing = Just (d1 + 150, "L" : path1)
    f2 Nothing (Just (d2, path2)) = Just (d2 + 150, "R" : path2)
    f2 Nothing Nothing            = Nothing

    f3 (Just (Right Diamonds)) = Just (0, [])
    f3 _ = Nothing
