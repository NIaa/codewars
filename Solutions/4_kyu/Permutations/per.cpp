#include <iostream>
#include <string>
#include <vector>

using namespace std;

vector<string> ins(vector<string> svec, const string letter){
    vector<string> ret;
    string tmp;
    for(vector<string>::iterator it = svec.begin(); it != svec.end(); ++it){
        for(int i = 0; i != it->size(); ++i){
            tmp = *it;
            ret.push_back(tmp.insert(i, letter));
        }
        tmp = *it;
        ret.push_back(tmp.insert(it->size(), letter));
    }
    sort(ret.begin(), ret.end());
    return vector<string> (ret.begin(), unique(ret.begin(), ret.end()));
}

vector<string> permutations(string s) {
    if(s.size() <= 1) return vector<string> (1, s); 
    else{
        string letter(1, *(s.end()-1));
        string s1(s.begin(), s.end()-1);
        return ins(permutations(s1), letter);
    }
}