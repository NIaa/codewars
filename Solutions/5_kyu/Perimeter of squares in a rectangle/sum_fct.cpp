typedef unsigned long long ull;
class SumFct
{
  public:
  static ull perimeter(int n)
  {
    std::vector<ull> fb(2,1);
    while(fb.size() != n+1) fb.push_back(*(fb.end()-1)+*(fb.end()-2));
    ull ret = 0;
    for(std::vector<ull>::iterator it = fb.begin(); it != fb.end(); ++it) ret+=4**it;
    return ret;
  }
};