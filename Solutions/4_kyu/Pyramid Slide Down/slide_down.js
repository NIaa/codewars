function longestSlideDown (p) {
  l=p[p.length-1]
  while(l.length!=1){
    buf=[]
    for(var i=0; i<l.length-1;i++){
      buf[i] = l[i]>l[i+1]?l[i]:l[i+1]
      buf[i] += p[l.length-2][i]
    }
    l=buf
  }
  return l[0]
}