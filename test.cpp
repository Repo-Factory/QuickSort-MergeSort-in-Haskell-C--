#include <iostream>
#include <vector>
#include <functional>
using namespace std;

bool greaterThan(int x, int y)
{
  return x > y;
}

bool lessThan(int x, int y)
{
  return x < y;
}

bool always(int x, int y)
{
  return true;
}

void printArray(vector<int>& vect)
{
  for (int element : vect)
  {
    std::cout << element << " ";
  }
  std::cout << std::endl;
}

int main()
{
  vector<int> unsortedList{5,2};
  printArray(unsortedList);
  return 0;
}
