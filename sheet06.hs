data Expr = Const Int      -- constant integer
          | Div Expr Expr  -- division of two expressions

eval :: Expr -> Int
eval (Const x) = x
eval (Div e1 e2) = eval e1 `div` eval e2

expr1 = (Const 2142 `Div` Const 3) `Div` Const 17
expr2 = Const 1 `Div` Const 0

--------------------------------------------------------

data Error a = Good a
             | Bad String
    deriving Show

--------------------------------------------------------

instance Functor Error where
  fmap f (Good x)   = Good (f x)
  fmap f (Bad s) = Bad s

instance Applicative Error where
  pure                  = Good
  (Good f) <*> (Good x) = Good (f x)
  (Bad s)  <*> _        = Bad s
  _        <*> (Bad s)  = Bad s

expr3 = Const 1 `Div` (Const 0 `Div` Const 42)

instance Monad Error where
    (Good x) >>= f = f x
    (Bad s)  >>= _ = Bad s

--------------------------------------------------------

data Writer a = Out String a
                    deriving Show

logCon x = show x ++ "\n"
logDiv x y z = show x ++ "/" ++ show y ++ "=" ++ show z ++ "\n"

prettyPrint (Out s r) = do
   putStrLn s
   putStrLn (show r)

--------------------------------------------------------

instance Functor Writer where
    fmap f (Out s x) =  Out s (f x)

instance Applicative Writer where
    pure = Out ""
    Out s1 f <*> Out s2 x = Out (s1++s2) (f x)

instance Monad Writer where
  return           = pure
  Out s1 x >>= f   = let Out s2 y = f x
                     in Out (s1 ++ s2) y

write :: String -> Writer ()
write s = Out s ()

--1a
evalW :: Expr -> Writer Int
evalW (Const n)    = write (logCon n) >> return n
evalW (Div e1 e2)  = do
    x1 <- evalW e1
    x2 <- evalW e2
    let x = x1 `div` x2
    write (logDiv x1 x2 x)
    return x

--1b
execWriter :: Writer a -> String
execWriter (Out s _) = s

--1c
fibWriter :: Int -> Writer Int
fibWriter 0 = write "0" >> return 0
fibWriter 1 = write "1" >> return 1
fibWriter n = do
    write "("
    a <- fibWriter (n-1)
    write "+"
    b <- fibWriter (n-2)
    write ")"
    return (a + b)

--------------------------------------------------------

data StateTrans s a = ST (s -> (a,s))

evalS :: Expr -> StateTrans Int Int
evalS (Const n)   = ST f where f c = (n,c)
evalS (Div e1 e2) = ST f
      where f c = let ST f1 = evalS e1
                      ST f2 = evalS e2
                      (x1,c1) = f1 c
                      (x2,c2) = f2 c1
                  in (x1 `div` x2, c2+1)

--------------------------------------------------------

instance Functor (StateTrans s) where
   fmap f (ST h) = ST (\s -> let (x,s') = h s in (f x, s'))


instance Applicative (StateTrans s) where
  pure x = ST (\s -> (x,s))
  (ST sf) <*> (ST sg) = ST (\s -> let (f,s')   = sf s
                                      (x',s'') = sg s'
                                  in (f x',s''))

instance Monad (StateTrans s) where
  return       = pure
  ST g >>= f   = ST (\s -> let (x,s') = g s
                               ST h   = f x
                           in h s')

incr :: StateTrans Int ()
incr = ST (\s -> ((), s+1))

evalS' :: Expr -> StateTrans Int Int
evalS' (Const n)    = return n
evalS' (Div e1 e2)  = do
    x1 <- evalS' e1
    x2 <- evalS' e2
    incr
    return (x1 `div` x2)

-- Usage: runST (evalS' expr1) 0
runST :: StateTrans s a -> s -> (a,s)
runST (ST f) s = f s

--------------------------------------------------------

type LogMsg = String

data EWS state a = EWS
  { runEWS :: state -> (Error a, LogMsg, state)}

--2a
getST :: StateTrans s s
getST = ST (\s -> (s,s))

--2b
putST :: s -> StateTrans s ()
putST s = ST (\_ -> ((), s))

--2c
modifyST :: (s -> s) -> StateTrans s ()
modifyST f = ST (\s -> ((), f s))

--2d
data Tree a = Node (Tree a) a (Tree a)
 | Empty
 deriving Show

example_tree = Node
 (Node Empty 5 Empty)
 7
 (Node (Node Empty 8 Empty) 9 Empty)


enumTree :: Tree a -> StateTrans Int (Tree Int)
enumTree Empty = return Empty
enumTree (Node left _ right) = do
    left' <- enumTree left   
    n <- getST              
    modifyST (+1)           
    right' <- enumTree right 
    return (Node left' n right')


--3a

data SWE s a = SWE {runSWE :: s -> (Error a, LogMsg, s)}

instance Functor (SWE s) where
    fmap f (SWE g) = SWE (\s -> 
        let (res, logMsg, newState) = g s
        in (fmap f res, logMsg, newState))


instance Applicative (SWE s) where
    pure x = SWE (\s -> (Good x, "", s))  

    (SWE f) <*> (SWE g) = SWE (\s -> 
        let (resF, log1, s1) = f s
            (resX, log2, s2) = g s1
        in case (resF, resX) of
            (Good f', Good x) -> (Good (f' x), log1 ++ log2, s2)
            (Bad err, _) -> (Bad err, log1 ++ log2, s2)
            (_, Bad err) -> (Bad err, log1 ++ log2, s2))

instance Monad (SWE s) where
    return = pure

    (SWE f) >>= g = SWE (\s -> 
        let (res, log1, s1) = f s
        in case res of
            Bad err -> (Bad err, log1, s1)  
            Good x  -> let SWE h = g x
                           (res', log2, s2) = h s1
                       in (res', log1 ++ log2, s2))  

--3b
evalSWE :: Expr -> SWE Int Int
evalSWE (Const n) = SWE (\s -> (Good n, show n ++ "\n", s))  

evalSWE (Div e1 e2) = SWE (\s -> 
    let SWE f1 = evalSWE e1
        SWE f2 = evalSWE e2
        (res1, log1, s1) = f1 s
        (res2, log2, s2) = f2 s1
    in case (res1, res2) of
        (Good x1, Good x2) -> 
            if x2 == 0
            then (Bad "Division durch 0", log1 ++ log2 ++ "Fehler: Division durch 0\n", s2)
            else (Good (x1 `div` x2), log1 ++ log2 ++ show x1 ++ " / " ++ show x2 ++ " = " ++ show (x1 `div` x2) ++ "\n", s2 + 1)
        (Bad err, _) -> (Bad err, log1 ++ log2, s2)  
        (_, Bad err) -> (Bad err, log1 ++ log2, s2))

--3c
runTests :: IO ()
runTests = do
    putStrLn "Test 1: Erfolgreicher Ausdruck"
    print $ runSWE (evalSWE expr1) 0
    
    putStrLn "\nTest 2: Fehlerhafter Ausdruck (Division durch 0)"
    print $ runSWE (evalSWE expr2) 0