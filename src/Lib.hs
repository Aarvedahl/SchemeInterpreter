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