std::string zoom(int n)
{
  std::string ret="";
  int mid=n/2;
  for(int i=0;i<n;++i){
    for(int j=0;j<n;++j){
      ret+=std::max(abs(mid-i),abs(mid-j))%2?"□":"■";
    }
    if(i!=n-1) ret+="\n";
  }
  return ret;
}