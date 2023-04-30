/* 
 * @author Conner Sommerfield 
 * QuickSort and MergeSort algorithms implemented in C++
 * Meant for comparison with Haskell interpretation
 * Generates 2 1000-number-sequences to be used as test inputs
 * Tests on reverse sorted sequences as well
 */

#include "sort.hpp"
  
/* We'll create our random items, then perform quickSort and mergeSort algorithms */
int main()
{
  vector<int> unsortedList = generateRandomNumbers(1000);
  vector<int> quickList  =  quickSort(unsortedList);
  vector<int> mergeList = mergeSort(unsortedList);
  printArray(quickList);
  printArray(mergeList);
  return 0;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
                                        //      QUICKSORT     //                                               
////////////////////////////////////////////////////////////////////////////////////////////////////////

/* 
 * I made the implementation as close to haskell implementation as possible. Uses operator overloading
 * to add smaller + pivot + larger.
*/
template <typename T>
vector<T> quickSort(vector<T>& list)
{
    // return empty list
    if (list.size() == 0)
    {
        return vector<T>{};
    }

    T listHead = list[0];                 // Pivot will be first element
    vector<T> listTail = getTail(list);   // Tail concept comes from haskell

    /* Sort the items less than the pivot and greater than the pivot recursively */
    vector<T> lessThan = lessThanEqual(listTail, listHead);
    vector<T> greaterThan = greaterList(listTail, listHead);
    vector<T> smallerSorted = quickSort(lessThan);
    vector<T> largerSorted  = quickSort(greaterThan);

    /* Replicate this line from haskell implementation ---> smallerSorted ++ [x] ++ biggerSorted */
    vector<T> mergeList = smallerSorted + listHead;
    return mergeList + largerSorted;  
}

/********************************************* QUICKSORT HELPERS ***********************************************/
/*
 * Comparator functions that add item to list if condition is met, helps creater smaller and larger lists
 * for quicksort algorithm 
 */
template <typename T>
vector<T> lessThanEqual(vector<T>& list, T listHead)
{
  vector<T> newList;
  for (T element : list)
  {
    if (element <= listHead)
    {
      newList.push_back(element);
    }
  }
  return newList;
}

template <typename T>
vector<T> greaterList(vector<T>& list, T listHead)
{
  vector<T> newList;
  for (T element : list)
  {
    if (element > listHead)
    {
      newList.push_back(element);
    }
  }
  return newList;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                                           //      MERGESORT     //                                               
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
 * Mimics the haskell implementation with 3 functions, mergeSort, split, and merge.  
 */
template <typename T>
vector<T> mergeSort(vector<T>& list)
{
    /* MergeSort will return empty or one-item lists. */
    if (list.size() == 0)
    {
        return vector<T>{};
    }
    if (list.size() == 1)
    {
        return list;
    }
    /* 
     * Bigger lists will be split into two parts, calling mergeSort recursively on the two parts
     * (actually very similar to the quicksort from above). Then it merges those two sorted lists to give final 
     * sorted list. 
     */
    pair<vector<T>, vector<T>> splitList = split(list);
    vector<T> left = splitList.first;
    vector<T> right = splitList.second;
    vector<T> sortedLeft = mergeSort(left);
    vector<T> sortedRight = mergeSort(right);
    return merge(sortedLeft, sortedRight);
}

/* 
 * To split list into two parts, we will call split which will find the middle and return list1 as the elements
 * before/including the middle, and list2 as the elements after the middle. 
 */
template <typename T>
pair<vector<T>, vector<T>> split(vector<T>& list)
{
  T middle = getMiddle(list);
  vector<T> list1 = copyList(list, 0, middle +  1);
  vector<T> list2 = copyList(list, middle + 1, list.size());
  pair<vector<T>, vector<T>> vectorPair;
  vectorPair.first = list1;
  vectorPair.second = list2;
  return vectorPair;
}

/* Merging will simply combine our two lists */
template <typename T>
vector<T> merge(vector<T>& list1, vector<T>& list2)
{
  /* An empty list makes our life easy because we just return the other one */
  if (list1.size() == 0 && list2.size() != 0)
  {
    return list2;
  }
  if (list1.size() != 0 && list2.size() == 0)
  {
    return list1;
  }

  T list1Head = list1[0];
  vector<T> list1Tail = getTail(list1);
  T list2Head = list2[0];
  vector<T> list2Tail = getTail(list2);

  /* 
   * Otherwise, we merge the lists by placing the lower of the two heads first and concatenating the rest of 
   * the list (thanks to operator overloading)
   */
  if (list1Head <= list2Head)
  {
    vector<T> mergeList = merge(list1Tail, list2);
    return list1Head + mergeList;
  }
  else  
  {
    vector<T> mergeList = merge(list1, list2Tail);
    return list2Head + mergeList;
  }
}
  
/********************************************* MERGESORT HELPERS ***********************************************/
/* Based on even or odd number of elements in vector, gives us the middle index */
template<typename T>
T getMiddle(vector<T>& list)
{
  T middle;
  if (list.size() % 2 == 0)
  {
    middle = list.size()/2 - 1;
  }
  else 
  {
    middle = list.size()/2;
  }
  return middle;
}

/* Copies all items of vector into new vector that are within range specified */
template <typename T>
vector<T> copyList(vector<T> list, int rangeStart, int rangeEnd)
{
  vector<T> newList;
  for (vector<int>::size_type i = (vector<int>::size_type) rangeStart; i < rangeEnd; i++)
  {
    newList.push_back(list[i]);
  }
  return newList;
}

/********************************************** GENERAL HELPERS ***********************************************/
/* Returns our random input sequence to be sorted */
vector<int> generateRandomNumbers(int numbers)
{
  vector<int> vect;
  for (vector<int>::size_type i = 0; i < numbers; i++)
  {
   vect.push_back(rand() % 100);
  }
  return vect;
}

/* Simple templated function to print all elements of vector */
template <typename T>
void printArray(vector<T>& vector)
{
  for (T element : vector)
  {
    std::cout << element << " ";
  }
  std::cout << std::endl;
}

/* 
 * Helps for haskell-like implementation, just takes all the elements besides the first of a vector and
 * places them into a new vector to be referred to as the tail
 */
template <typename T>
vector<T> getTail(vector<T>& list)
{
    vector<T> listTail;
    for (vector<int>::size_type i = 1; i < list.size(); i++)
    {
      listTail.push_back(list[i]);
    }
    return listTail;
}

/*
 * These operator overloading functions help us easily add lists or items to lists like haskell lets us 
 * do natively. There is one for merging a vector and a vector, one for prepending an item to a vector
 * with the addition sign, or for adding a item to the back using the addition sign
 */
template <typename T>
vector<T> operator+(vector<T>& lhs, vector<T>& rhs)
{
  /* Push back elements of two vectors into one vector */
  vector<T> newList;
  for (T element : lhs)
  {
    newList.push_back(element);
  }
  for (T element : rhs)
  {
    newList.push_back(element);
  }
  return newList;
}

template <typename T>
vector<T> operator+(T lhs, vector<T>& rhs)
{
  /* Prepend item to a vector */
  vector<T> newList;
  newList.push_back(lhs);
  for (T element : rhs)
  {
    newList.push_back(element);
  }
  return newList;
}

template <typename T>
vector<T> operator+(vector<T>& lhs, T rhs)
{
  /* Postpend item to a vector */
  vector<T> newList;
  for (T element : lhs)
  {
    newList.push_back(element);
  }
  newList.push_back(rhs);
  return newList;
}
/***************************************************************************************************************/
