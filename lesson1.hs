module MyBool where

import Prelude(Int(..), Show(..), Eq(..), Ord(..), (++))

data Date = Date Int Int String

data Year = Year Int deriving(Show, Eq, Ord) 

data Day = Day Int

data Month = January | Febrary | March | April


--instance Show Year where
--   show (Year x) =  show x


instance Show Date where
  show (Date y m d) = show d ++ " " ++ show m ++ " " ++ show y


instance Show Month where
   show January =  "January"
   show Febrary =  "Febrary"
   show March =  "March"
   show April =  "April"


data Bool = True | False

true :: Bool
true = True

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and False _ = False
and True x = x

or :: Bool -> Bool -> Bool
or True _ = True
or False x = x

