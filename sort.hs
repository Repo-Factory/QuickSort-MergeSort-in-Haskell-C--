 -- @author Conner Sommerfield 
 -- QuickSort and MergeSort algorithms implemented in haskell
 -- Meant for comparison with C++ interpretation
 -- Generates 2 1000-number-sequences to be used as test inputs
 -- Tests on reverse sorted sequences as well
 
main :: IO ()
main = 
   
    do print( quicksort [7,6,5] )
       print( mergesort [9,5,4] )

-- quicksort in haskell is very minimal
quicksort :: (Ord list) => [list] -> [list]
quicksort [] = []                          -- return empty list
quicksort (x:xs) =                         -- else pivot is head, sort all items less/greater than pivot recursively
  let smallerSorted = quicksort [list | list <- xs, list <= x]
      biggerSorted  = quicksort [list | list <- xs, list > x]
  in  smallerSorted ++ [x] ++ biggerSorted -- merge the two sorted lists with pivot in middle
 

-- mergeSort implementation uses 3 functions, one to sort, one to split, one to merge
mergesort :: Ord list => [list] -> [list]
mergesort [] = []
mergesort [x] = [x]                                 -- return empty or one-item lists
mergesort xs =
  let (left, right) = split xs                      -- else sort the split left and right lists 
  in merge (mergesort left) (mergesort right)       -- merge the two sorted lists

split :: [list] -> ([list], [list])                 -- Call splitAt which returns two lists, one with elements
split xs = splitAt (length xs `div` 2) xs           -- before middle, one with the elements after middle

merge :: Ord list => [list] -> [list] -> [list]
merge [] ys = ys                                    -- Merging empty list means just return non-empty list
merge xs [] = xs
merge (x:xs) (y:ys)                                 -- Else check which head is smaller
  | x <= y    = x : merge xs (y:ys)                 -- We'll put that element first in our mergelist, then the rest
  | otherwise = y : merge (x:xs) ys