{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Demo.TypeSafety
-- Copyright   :
-- License     :  BSD3
--
--
-- |
--
-----------------------------------------------------------------------------

module Demo.TypeSafety (

) where

#ifdef SAMPLE_CODE_ONLY

data Bool = False | True
data Maybe a = Nothing | Just a
data [a] = [] | a : [a]

square :: Int -> Int
square x = x * x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

putStrLn :: String -> IO ()
clearScreen :: IO ()

-- > Just True
-- Just True :: Maybe Bool
-- > 1 : 2 : 3 : []
-- [1,2,3]
-- > square 4
-- 16 :: Int
-- > replicate 3 False
-- [False,False,False] :: [Bool]

plus :: Int -> Int -> Int
plus 1 "x" -- fail!

Int#
Double#
ByteArray#
Addr#
Bool

True   :: Bool
I# 123 :: Int
x:xs   :: [a]

plus :: Int -> Int -> Int
plus (I# x) (I# y) = I# (plusUnboxed x y)

plusUnboxed :: Int# -> Int# -> Int#

intBox :: Int# -> Int

indexByteArray_Double# :: ByteArray# -> Int# -> Double#

unBox :: Int -> Int#
unBox (I# i) = i

bool :: Bool -> a -> a -> a
bool cond x y = case cond of
                  False -> x
                  True  -> y

-- > [1..10] -- enumFromTo 1 10
-- [1,2,3,4,5,6,7,8,9,10] :: [Int]
--
-- > sum [1..10]
-- 55 :: Int

sumAndProduct1 :: Int -> Int
sumAndProduct1 n = sum [1..n] + product [1..n]

sumAndProduct2 :: Int -> Int
sumAndProduct2 n = let xs = [1..n]
                   in  sum xs + product xs

plus :: Int -> Int -> Int
plus (I# x) (I# y) = I# (plusUnboxed x y)

Int -> Int -> Int ~ Int -> (Int -> Int)

-- > plus 1 2
-- 3 :: Int
-- > plus 1
-- Function :: Int -> Int

plusOne :: Int -> Int
plusOne x = plus 1 x

plusOne :: Int -> Int
plusOne = plus 1

map :: (a -> b) -> [a] -> [b]

-- > map plusOne [1,2,3]
-- [2,3,4] :: [Int]
-- > map plus [1,2,3]
-- [Function,Function,Function] :: [Int -> Int]
-- > map (plus 1) [1,2,3]
-- [2,3,4]

class Show a where
  show :: a -> String

instance Show Int where show x = showInt

i :: Int
i = 123

-- > show i
-- "123" :: String

showTwice :: Show a => a -> String
showTwice a = let xs = show a
              in  append xs xs

-- > showTwice i
-- "123123" :: String

data Show a = Show (a -> String)

show :: Show a -> a -> String

show_Int = Show showInt

showTwice :: Show a -> a -> String


   listOfNumbers :: Int -> [Int]
   listOfNumbers n = n : listOfNumbers (n+1)

-- > 1 : 2 : 3 : []
-- [1,2,3] :: [Int]

-- > listOfNumbers 2
-- [2,3,4,5,6,7,8,9,10,11,....... :: [Int]

-- > map square (listOfNumbers 2)
-- [4,9,16,25,36,49,64,81,..... :: [Int]

-- > take 5 (map square (listOfNumbers 2))
-- [4,9,16,25,36]



---------------------------------------------------

    -- > 1 : 2 : 3 : []
    -- [1,2,3]


    xs !! n = head (drop n xs)

    -- > [2,4,8,16] !! 2
    -- 8 :: Int



    fromMaybe :: a -> Maybe a -> a
    fromMaybe d m = case m of
                      Nothing -> d
                      Just x  -> x

    -- > fromMaybe 123 Nothing
    -- 123 :: Int

    -- > fromMaybe 123 (Just 234)
    -- 234 :: Int


    repeat x :: a -> [a]
    repeat x = x : repeat x

    -- > repeat 10
    -- [10,10,10,10,10,10,10,10...  :: [Int]

    -- > [1..]
    -- [1,2,3,4,5,6,7,8,9,10,11...  :: [Int]

    -- > map square [1..]
    -- [1,4,9,16,25,36,49,64,81...  :: [Int]

    -- > take 5 [1..]
    -- [1,2,3,4,5] :: [Int]

    -- > zip [1..] [True,True,False]
    -- [(1,True), (2,True), (3,False)] :: [(Int,Bool)]


    bool :: Bool -> a -> a -> a
    bool c x y = case c of
                   True  -> x
                   False -> y

    or :: Bool -> Bool -> Bool
    or x y = bool x True y

    and :: Bool -> Bool -> Bool
    and x y = bool x y False


#endif

