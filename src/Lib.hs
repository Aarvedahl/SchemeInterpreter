module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bookinfo book = "Your book is called  " ++ book

bmiTell weight height = bmiInfo (weight/height)

bmiInfo bmi
  | bmi <= skinny = "You are underweight"
  | bmi <= normal = "You are normal"
  | bmi <= fat = "You are fat"
  | otherwise = "You are a whale"
  where skinny = 18.5
        normal = 25
        fat = 30


cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea


describeList xs = "The list is " ++ what xs
  where what [] = "empty"
        what [x] = "A singleton list"
        what xs = "A really long list"


fibonacci number = case number of 0 -> 1
                                  1 -> 1
                                  number -> fibonacci (number -2) + fibonacci (number -1)


maxi2 [] = error "maximum of empty list"
maxi2 [x] = x
maxi2 (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maxi2 xs

repli n x
  | n <= 0 = []
  | otherwise = x:repli (n-1) x

take2 n _
  | n <= 0 = []
take2 _ [] = []
take2 n (x:xs) = x:take2 (n-1) xs

rever [] = []
rever (x:xs)  = rever xs ++ [x]

zippit _ [] = []
zippit [] _ = []
zippit (x:xs) (y:ys) = (x, y):zippit xs ys

reverSort [] = []
reverSort xs =
  rever $ quicksort (xs)

elems a [] = False
elems a (x:xs)
  | a == x = True
  | otherwise = a `elems` xs

multiplyList (x:xs)
  | x == 0 = []
  | otherwise = x * x:multiplyList xs


fib 0 = 1
fib 1 = 1
fib n
  | n >= 2 = fib (n-1) + fib (n-2)

divideByTen = (/10)

isUpperLetter = (`elem` ['A'..'Z'])

subtract4 = (subtract 4)

applyTwice f x = f (f x)

quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a|a <- xs, a<=x]
      biggerSorted = quicksort [a|a<-xs, a>x]
  in smallerSorted ++ [x] ++ biggerSorted


compareWithHundred = compare 100
