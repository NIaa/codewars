function sortMyString(S) {
    var even=Array(), odd=Array()
    for(var i=0; i<S.length; ++i){
      if(i%2==0) even[parseInt(i/2)]=S[i]
      else       odd [parseInt(i/2)]=S[i]
    }
    //f = function(a,b){return a.toLowerCase()>b.toLowerCase()?1:-1;}
    //return even.sort(f).join("")+" "+odd.sort(f).join("")
    return even.join("")+" "+odd.join("")
}