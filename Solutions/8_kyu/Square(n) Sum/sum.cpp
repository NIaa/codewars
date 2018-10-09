#include <vector>

int square_sum(const std::vector<int>& numbers)
{
  int sum=0;
  for(size_t i=0; i!=numbers.size(); ++i) sum+=numbers[i]*numbers[i];
  return sum;
}