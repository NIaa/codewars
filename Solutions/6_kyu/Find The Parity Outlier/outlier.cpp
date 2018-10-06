int FindOutlier(std::vector<int> arr)
{
    int result = 0;
    int odd = 0;
    for(std::vector<int>::iterator it = arr.begin(); it != arr.end(); ++it)
    {
      if((*it)%2) ++odd;
    }
    for(std::vector<int>::iterator it = arr.begin(); it != arr.end(); ++it)
    {
      if(odd>=2)
      {
        if(!((*it)%2)) result = *it;
      }
      else
      {
        if((*it)%2) result = *it;
      }
    }
    return result;
}