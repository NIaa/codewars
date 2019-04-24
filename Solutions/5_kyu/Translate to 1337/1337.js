// Can this be even more rediculous?
function toLeet(str) {
  var ret = "";
  var counter = [];for(var i = 0; i < 26; i++) counter[i] = 0;
  for(var i=0; i<str.length; i++)
  {
    var c = str[i];
    counter[c.charCodeAt()-"a".charCodeAt()]+=1;
    if(c=='e') ret+="3";
    else if(c=='f') ret+="|=";
    else if(c=='j') ret+="_|";
    else if(c=='l') ret+="|_";
    else if(c=='n') ret+="|\\|";
    else if(c=='o') ret+="0";
    else if(c=='q') ret+="(,)";
    else if(c=='y') ret+="`/";
    else if(c=='z') ret+="(\\)";
    else if(c=='a') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "4" : "@";
    else if(c=='b') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|3" : "8";
    else if(c=='d') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|)" : "o|";
    else if(c=='g') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "9" : "6";
    else if(c=='k') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|<" : "|{";
    else if(c=='p') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|2" : "|D";
    else if(c=='r') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|Z" : "|?";
    else if(c=='s') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "5" : "$";
    else if(c=='t') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "+" : "7";
    else if(c=='v') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "|/" : "\\/";
    else if(c=='w') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "\\^/" : "//";
    else if(c=='x') ret+=counter[c.charCodeAt()-"a".charCodeAt()]%2 ? "><" : "}{";
    else if(c=='h'){
      if(counter[c.charCodeAt()-"a".charCodeAt()]%6==1) ret+="|-|";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%6==2) ret+="]-[";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%6==3) ret+="}-{";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%6==4) ret+="(-)";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%6==5) ret+=")-(";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%6==0) ret+="#";
    }
    else if(c=='i'){
      if(counter[c.charCodeAt()-"a".charCodeAt()]%3==1) ret+="1";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%3==2) ret+="!";
      else if(counter[c.charCodeAt()-"a".charCodeAt()]%3==0) ret+="][";
    }
    else ret+=str[i]
  }
  return ret;
}