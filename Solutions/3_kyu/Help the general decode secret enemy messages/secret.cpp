using namespace std;
struct Decoder {
  static string decode (const string& p_what) {  
    string table("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,? ");
    static string ret;
    ret="";
    for(int i=0; i!=p_what.size(); ++i){
      if(table.find(p_what[i])<=65){
        for(int j=0; j!=66; ++j){
          if(Encoder::encode(string(i%66,'_')+table[j])[i%66]==p_what[i]){  
            ret+=table[j];
            break;
          }
        }
      }
      else ret+=p_what[i];
    }
    return ret;
  }
};