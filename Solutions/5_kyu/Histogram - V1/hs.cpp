using std::string;
string histogram(std::vector<int> results)
{
  std::vector<string> row_vec;
  string row;
  int max=results[0];
  size_t sz=results.size();
  for(size_t i=0; i!=sz; ++i){
    row+=(char)(i+'1');
    row+=" ";
    max=(results[i]>max)?results[i]:max;
  }
  row_vec.push_back(row);
  row="";
  for(size_t i=0; i!=sz; ++i){
    row+=(i==sz-1)?"-":"--";
  }
  row_vec.push_back(row);
  if(max!=0){
  for(int h=0; h<=max; ++h){
    row="";
    for(size_t i=0; i<sz; ++i){
      if(results[i]>=h+1) row+="# ";
      else if(results[i]==h && h!=0){
        if(results[i]/10==0){
          row+=(char)(h+'0');
          row+=" ";
        }
        else{
          row+=(char)(results[i]/10+'0');
          row+=(char)(results[i]%10+'0');
        }
      }
      else row+="  ";
    }
    row_vec.push_back(row);
  }
  }
  string ret;
  for(size_t i=0; i!=row_vec.size(); ++i){
    row_vec[i].erase(row_vec[i].find_last_not_of(" ")+1);
    row_vec[i]+='\n';
    }
  for(size_t i=row_vec.size()-1; i!=-1; --i) ret+=row_vec[i]; 
  return ret;
}