main :: IO ()
main = 
    -- takes the first item of the list, and makes a new list that has all the numbers less than it first, then itself 
    -- and all the numbers greater than it after. Doing so recursively sorts the whole list.
    do print( quicksort [7,6,5] )

quicksort :: (Ord list) => [list] -> [list]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [list | list <- xs, list <= x]
      biggerSorted  = quicksort [list | list <- xs, list > x]
  in  smallerSorted ++ [x] ++ biggerSorted
 

mergesort :: Ord list => [list] -> [list]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
  let (left, right) = split xs
  in merge (mergesort left) (mergesort right)

split :: [list] -> ([list], [list])
split xs = splitAt (length xs `div` 2) xs

merge :: Ord list => [list] -> [list] -> [list]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys