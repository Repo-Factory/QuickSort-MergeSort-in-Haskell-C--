#include <iostream>
#include <vector>
#include <functional>
#include <utility>
#include <ctime>
#include <random>
#include <algorithm>

using namespace std;

/* Quick Sort */
template <typename T>
vector<T>                   quickSort(vector<T>& list);
/* Quick Sort Helpers */
template <typename T>
vector<T>                   lessThanEqual(vector<T>& list, T listHead);
template <typename T>
vector<T>                   greaterList(vector<T>& list, T listHead);


/* Merge Sort */
template <typename T>
vector<T>                   mergeSort(vector<T>& list);
template <typename T>
pair<vector<T>, vector<T>>  split(vector<T>& list);
template <typename T>
vector<T>                   merge(vector<T>& list1, vector<T>& list2);
/* Merge Sort Helpers */
template<typename T>
T                           getMiddle(vector<T>& list);
template <typename T>
vector<T>                   copyList(vector<T> list, int rangeStart, int rangeEnd);


/* General Helpers */
template <typename T>
vector<T>                   getTail(vector<T>& list);
template <typename T>
void                        printArray(vector<T>& vector);
vector<int>                 generateRandomNumbers(int numbers);

template <typename T>
vector<T>                   operator+(vector<T>& lhs, vector<T>& rhs);
template <typename T>
vector<T>                   operator+(T lhs, vector<T>& rhs);
template <typename T>
vector<T>                   operator+(vector<T>& lhs, T rhs);
