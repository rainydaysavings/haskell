-----------------------------------------------------------
-----------------  EXERCISES HASKELL '19  -----------------
-----------------------------------------------------------

import Data.Char
import Data.List

--Aula 1---------------------------------------------------
min3:: Int -> Int -> Int -> Int
min3 a b c = minimum [a, b, c]

tresiguais:: Int -> Int -> Int -> Bool
tresiguais a b c = if a == b && b == c then True else False

media:: Float -> Float -> Float
media a b = (a + b)/2

par:: Int -> Bool
par a = if  a`mod`2 == 0 then True else False

sinal:: Int -> Int
sinal a = if a >= 0 then 1 else -1

--2.-------------------------------------------------------
triangle:: Int -> Int -> Int -> Bool
triangle a b c = (a + b) < c 

--3.-------------------------------------------------------
areaTriangle:: Float -> Float -> Float -> Float
areaTriangle a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2

--4.-------------------------------------------------------
halfs:: [Int] -> ([Int], [Int])
halfs n = (take half n, drop half n)
    where half = div (length n) 2

--5a.-------------------------------------------------------
selectlast:: [Int] -> Int
selectlast n = n !! realLength
    where realLength = (length n) - 1

--5b.-------------------------------------------------------
droplast:: [Int] -> [Int]
droplast n = take realLength n
    where realLength = (length n) - 1

--6a.--------------------------------------------------------
binom:: Int -> Int -> Int
binom n k = div (product[1..n]) ((product [1..k]) * product [1..(n-k)])

--6b.--------------------------------------------------------


--7a.--------------------------------------------------------
-- O min3 já foi feito --------------------------------------
-------------------------------------------------------------
min3':: Integer -> Integer -> Integer -> Integer
min3' a b c = min a (min b c)

max3:: Int -> Int -> Int -> Int
max3 a b c = maximum [a, b, c]

-- OR -------------------------------------------------------
max3':: Integer -> Integer -> Integer -> Integer
max3' a b c = max a (max b c)

--8a.--------------------------------------------------------
maxOccurs:: Integer -> Integer -> (Integer, Integer)
maxOccurs a b | a == b = (a, 2)
              | otherwise = (max a b, 1)

--8b.--------------------------------------------------------
mid3:: Integer -> Integer -> Integer -> Integer
mid3 a b c = a+b+c - ((max3' a b c)+(min3' a b c))

orderTriple:: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a, b, c) = (min3' a b c, mid3 a b c, max3' a b c)

--9.---------------------------------------------------------
classify:: Int -> String
classify a | a <= 9    = "Bad"
           | a <= 12   = "Ok"
           | a <= 15   = "Good"
           | a <= 18   = "Great"
           | otherwise = "Excellent"

--10.--------------------------------------------------------
xor:: Bool -> Bool -> Bool
xor a b = if (a /= b) then True else False

--11.--------------------------------------------------------
safetail:: [a] -> [a]
safetail a  | length a == 0 = []
            | otherwise = tail a

--12a.--------------------------------------------------------
short:: [a] -> Bool
short a = if (length a < 3) then True else False

--12b.--------------------------------------------------------
short':: [a] -> Bool
short' [] = True
short' [_] = True
short' [_,_] = True
short' a = False

--13.---------------------------------------------------------
decimal:: Int -> String
decimal n = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] !! n

teens:: Int -> String
teens n = ["", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eightteen", "nineteen"] !! (n`mod`10)

tens:: Int -> String
tens n = ["", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! n

textual:: Int -> String
textual n | n < 10 = decimal n
          | n < 20 = teens n
          | n < 100 = tens (div n 10) ++ " " ++ decimal (mod n 10)
          | n < 1000 = decimal (div n 100) ++ " hundred and " ++ tens (div (mod n 100) 10) ++ " " ++ decimal (mod (mod n 100) 10)

--22.----------------------------------------------------------
sumlist:: Int -> Int
sumlist n = sum [x^2 | x <- [1..n]]

--23a.---------------------------------------------------------
aprox:: Int -> Double
aprox n = 4 * sum [((-1)^(fromIntegral x)/(2*(fromIntegral x) + 1)) | x <- [0..n]]

--23b.---------------------------------------------------------
aproxfast:: Int -> Double
aproxfast n = sqrt(12 * sum [((-1)^(fromIntegral x)/(((fromIntegral x)+1)^2)) | x <- [0..n]])

--24.----------------------------------------------------------
divprop:: Int -> [Int]
divprop n = [ x | x <- [1..n-1], mod n x == 0]

--25.----------------------------------------------------------
perfects:: Int -> [Int]
perfects n = [ x | x <- [1..n-1], sum(divprop x) == x]

--26.----------------------------------------------------------
prime:: Int -> Bool
prime n = if length(divprop n) == 1 then True else False

--27.----------------------------------------------------------
pascal:: Int -> [[Int]]
pascal n = [ [binom a b | b <- [0..a]] | a <- [0..n]]

--28.----------------------------------------------------------
dotprod:: [Float] -> [Float] -> Float
dotprod i j = sum [x * y | (x,y) <- zip i j]

--29.----------------------------------------------------------
pitagorical:: Int -> [(Int, Int, Int)]
pitagorical n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2==z^2]

--30a.----------------------------------------------------------
factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--30b.----------------------------------------------------------
rangeProduct:: Int -> Int -> Int
rangeProduct n m | n == m = m 
                 | otherwise = n * rangeProduct (n+1) m

--30c.----------------------------------------------------------
factorialAutism:: Int -> Int 
factorialAutism 0 = 1
factorialAutism n = rangeProduct 1 n

--31.-----------------------------------------------------------
additionAddition:: Int -> Int -> Int 
additionAddition _ 0 = 0
additionAddition n m = n + additionAddition n (m-1)

--32.-----------------------------------------------------------
integerSqrt:: Int -> Int
integerSqrt n = sqrtaux 0 n

sqrtaux:: Int -> Int -> Int
sqrtaux k n | k^2 > n = k - 1
            | k^2 == n = k
            | otherwise = sqrtaux (k+1) n

--33.-----------------------------------------------------------
maxFun:: (Integer -> Integer) -> Integer -> Integer
maxFun j 0 = j 0
maxFun j n = max (j n) (maxFun j (n-1))

--34.-----------------------------------------------------------
anyZero:: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 = f 0 == 0
anyZero f n = f n == 0 || anyZero f (n-1)

--35.-----------------------------------------------------------
f1:: Integer -> Integer
f1 x = x

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f1 0 = 0
sumFun f 1 = f 1
sumFun f1 n = (sumFun f1 (n-1)) + (f1 n)

--36.-----------------------------------------------------------
mdc :: Integral a => a -> a -> a
mdc a b | b == 0 = abs a
        | otherwise = mdc b (a `mod` b)

--37.-----------------------------------------------------------
power:: Int -> Int -> Int
power a 0 = 1
power a b = a * power a (b-1)

--38a.----------------------------------------------------------
and':: [Bool] -> Bool
and' (x:[]) = x
and' (x:xs) = x && and' xs

--38b.----------------------------------------------------------
or':: [Bool] -> Bool
or' (x:[]) = x 
or' (x:xs) = x || or' xs

--38c.----------------------------------------------------------
concat':: [[a]] -> [a]
concat' (x:[]) = x
concat' (x:xs) = x ++ concat xs

--38d.----------------------------------------------------------
myReplicate :: Int -> a -> [a]
myReplicate 0 y = []
myReplicate x y = y : myReplicate(x-1) y

--39.-----------------------------------------------------------
-- WIP ---------------------------------------------------------
----------------------------------------------------------------

--40.-----------------------------------------------------------
strong:: String -> Bool
strong cs = and [a,b,c,d]
  where a = if (length [c | c<-cs, isLower c] > 0) then True else False
        b = if (length [c | c<-cs, isUpper c] > 0) then True else False
        c = if (length [c | c<-cs, isDigit c] > 0) then True else False
        d = if (length cs >= 8) then True else False

-- OR ----------------------------------------------------------
forte :: String -> Bool
forte xs = length xs >= 8 && length[c | c <- xs, isLower c] > 0 && length [c | c <- xs, isUpper c] > 0 && length [c | c <- xs, isDigit c] > 0

--41.-----------------------------------------------------------
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

