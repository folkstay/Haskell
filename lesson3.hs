--1
myLength :: [a] -> Int 
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--2
sumList :: [Int] -> Int 
sumList [] = 0
sumList(x:xs) = x + sumList xs

--3
productList :: [Int] -> Int 
productList [] = 1
productList(x:xs) = x * productList xs

--4
maximumList :: [Int] -> Int
maximumList[] = error "empty list"
maximumList [x] = x
maximumList (x:xs) = max x (maximumList xs)

--5
minimumList :: [Int] -> Int
minimumList[] = error "empty list"
minimumList [x] = x
minimumList (x:xs) = min x (minimumList xs)

--6
contains :: Eq a => a -> [a] -> Bool
contains _[] = False
contains y (x:xs) = y == x || contains y xs

--7
indexOf :: Eq a => a -> [a] -> Int
indexOf target list = helper 0 list
  where
    helper _ [] = -1 
    helper idx (x:xs)
      | target == x = idx 
      | otherwise = helper (idx + 1) xs

--8
reverseList :: [a] -> [a]
reverseList list = helper [] list
  where
    helper acc [] = acc
    helper acc (x:xs) = helper (x:acc) xs

--9
takeList :: Int -> [a] -> [a]
takeList n _ | n <=0 = []
takeList _ [] = []
takeList n (x:xs) = x : takeList (n - 1) xs

--10
dropList :: Int -> [a] -> [a]
dropList n xs | n <= 0 = xs
dropList _ [] = []
dropList n (_:xs) = dropList (n - 1) xs