/* In this task, the student is required to implement QuickSort and MergeSort algorithms using
 Haskell and another language of their choice. 
 The implementation should be tested on a random input of 1,000 numbers and two specific input
  sequences to ensure correctness. The student should also incorporate timing mechanisms into their code 
  to measure the performance of each algorithm.

After running the programs, the student should evaluate the results based on code readability,
 modularity, and maintainability. They should compare the performance of each algorithm on the
  sorted and reverse sorted input sequences and explain their findings.

The student should also summarize the differences and similarities between the languages used for 
sorting and state their preference if given the task of implementing Merge Sort again with any language
 of their choice. */


#include <iostream>
#include <vector>
#include <functional>
#include <utility>
#include <ctime>
#include <random>
#include <algorithm>


using namespace std;

vector<int> quickSort(vector<int>& list);
vector<int> merge(vector<int>& list1, vector<int>& list2);
vector<int> mergeSort(vector<int>& list); // , const function <bool (int, int)>& comparatorFunction
pair<vector<int>, vector<int>> split(vector<int>& list);
vector<int> operator+(int lhs, vector<int>& rhs);



void printArray(vector<int>& vector)
{
  for (int element : vector)
  {
    std::cout << element << " ";
  }
  std::cout << std::endl;
}

int main()
{
  std::srand(unsigned(std::time(nullptr)));
  std::vector<int> v(1000);
  std::generate(v.begin(), v.end(), std::rand);

  vector<int> unsortedList = vector<int>{7,6,5,4,3,2,1, 5, 9, 11, 64, 32, 44};
  vector<int> sortedList  =  quickSort(v);
  vector<int> mergeSortList = mergeSort(v);
  printArray(sortedList);
  cout << "H0rray!" << endl;
  printArray(mergeSortList);
  return 0;
}

vector<int> quickSort(vector<int>& list) // , const function <bool (int, int)>& comparatorFunction
{
    if (list.size() == 0)
    {
        return vector<int>{};
    }

    int listHead = list[0];       // pivot
    vector<int> listTail;
    for (vector<int>::size_type i = 1; i < list.size(); i++)
    {
      listTail.push_back(list[i]);
    }
  
    vector<int> lessThan;
    vector<int> greaterThan;
    for (int element : listTail)
    {
      if (element <= listHead)
      {
        lessThan.push_back(element);
      }
      if (element > listHead)
      {
        greaterThan.push_back(element);
      }
      
    }

    vector<int> smallerSorted = quickSort(lessThan); // [](){x < }
    vector<int> largerSorted  = quickSort(greaterThan);
    vector<int> mergeList;

    for (int element : smallerSorted)
    {
      mergeList.push_back(element);
    }
    mergeList.push_back(listHead);
    for (int element : largerSorted)
    {
      mergeList.push_back(element);
    }
    return mergeList;
    
}

vector<int> mergeSort(vector<int>& list) // , const function <bool (int, int)>& comparatorFunction
{
    if (list.size() == 0)
    {
        return vector<int>{};
    }
    if (list.size() == 1)
    {
        return list;
    }
    pair<vector<int>, vector<int>> splitList = split(list);
    vector<int> left = splitList.first;
    vector<int> right = splitList.second;
    vector<int> sortedLeft = mergeSort(left);
    vector<int> sortedRight = mergeSort(right);
    return merge(sortedLeft, sortedRight);
}

pair<vector<int>, vector<int>> split(vector<int>& list)
{
  int middle;
  if (list.size() % 2 == 0)
  {
  middle = list.size()/2 - 1;
  }
  else {
  middle = list.size()/2;
  }
  vector<int> list1;
  vector<int> list2;
  for (vector<int>::size_type i = 0; i <= middle; i++)
  {
    list1.push_back(list[i]);
  } 
  for (vector<int>::size_type i = middle + 1; i < list.size(); i++)
  {
    list2.push_back(list[i]);
  }
  pair<vector<int>, vector<int>> vectorPair;
  vectorPair.first = list1;
  vectorPair.second = list2;
  return vectorPair;
}

vector<int> merge(vector<int>& list1, vector<int>& list2)
{
  if (list1.size() == 0 && list2.size() != 0)
  {
    return list2;
  }
  if (list1.size() != 0 && list2.size() == 0)
  {
    return list1;
  }
  int list1Head = list1[0];
  vector<int> list1Tail;
  for (vector<int>::size_type i = 1; i < list1.size(); i++)
  {
    list1Tail.push_back(list1[i]);
  }
  int list2Head = list2[0];
  vector<int> list2Tail;
  for (vector<int>::size_type i = 1; i < list2.size(); i++)
  {
    list2Tail.push_back(list2[i]);
  }
  if (list1Head <= list2Head)
  {
    vector<int> mergeList = merge(list1Tail, list2);
    return list1Head + mergeList;
  }
  else
  {
    vector<int> mergeList = merge(list1, list2Tail);
    return list2Head + mergeList;
  }
}

vector<int> operator+(int lhs, vector<int>& rhs)
{
  vector<int> newList;
  newList.push_back(lhs);
  for (int element : rhs)
  {
    newList.push_back(element);
  }
  return newList;
}

/* mergesort :: Ord list => [list] -> [list]
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
  | otherwise = y : merge (x:xs) ys */

/* void swap(int &a,int &b)
{
    int temp = a;
    a = b;
    b = temp;
}
int partition (int A[], int p, int r)
{
    int x = A[r];
    int i = p - 1;
 
    for (int j = p; j <= r- 1; j++)
    {
        if (A[j] <= x)
        {
            i++;
            swap (A[i], A[j]);
        }
    }
    swap (A[i + 1], A[r]);
    return (i + 1);
}
 
void quickSort(int A[], int p, int r)
{
    if (p < r)
    {
        int q = partition(A, p,r);
        quickSort(A, p, q - 1);
        quickSort(A, q + 1, r);
    }
}
 */

/* quicksort :: (Ord list) => [list] -> [list]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [list | list <- xs, list <= x]
      biggerSorted  = quicksort [list | list <- xs, list > x]
  in  smallerSorted ++ [x] ++ biggerSorted
 */