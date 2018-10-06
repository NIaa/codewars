class RGBToHex
{
  public:
  static std::string rgb(int r, int g, int b)
  {
    r=r<0?0:r;
    r=r>255?255:r;
    g=g<0?0:g;
    g=g>255?255:g;
    b=b<0?0:b;
    b=b>255?255:b;
    std::string ret;
    ret.push_back(to_str(r/16));
    ret.push_back(to_str(r%16));
    ret.push_back(to_str(g/16));
    ret.push_back(to_str(g%16));
    ret.push_back(to_str(b/16));
    ret.push_back(to_str(b%16));
    return ret;
  }
  private:
  static char to_str(int n)
  {
    if(n<10)return n+'0';
    else return (n-10+'A');
  }
};