var moveZeros = function (arr) {
    ret=[]
    zeros=0
    for(var i in arr){
      if(typeof(arr[i])==typeof(0) && arr[i]==0) zeros+=1
      else ret[ret.length]=arr[i]
    }
    for(var i=0; i<zeros; i++){
      ret[ret.length]=0
    }
    return ret
}