function chessValidator(board) {
    a=[]; for(var i=0; i<10; i++){ a[i]=[]; for(var j=0; j<9; j++) a[i][j]=0 }
    dic={
        '帥':-1, '仕':-2, '相':-3, '傌':-4, '俥':-5, '炮':-6, '兵':-7,
        '將': 1, '士': 2, '象': 3, '馬': 4, '車': 5, '砲': 6, '卒': 7
    }
    counter={};  for(var i=-7;i<=7;i++) counter[i]=0 
    row = board.split("\n")
    for(var i=0; i<10; i++) for(var j=0; j<9; j++) if(dic[row[i][j]]) a[i][j]=dic[row[i][j]]      
    for(var i=0; i<10; i++){
        for(var j=0; j<9; j++){
            counter[a[i][j]]+=1
            if(a[i][j]==1)  if(i>2 || j<3 || j>5) return false 
            if(a[i][j]==-1) if(i<7 || j<3 || j>5) return false   
            if(a[i][j]==1)  {gi=i; gj=j}
            if(a[i][j]==-1) {ri=i; rj=j}
            if(a[i][j]==2)  if(!((i==0 && (j==3||j==5)) || (i==1 && j==4) || (i==2 && (j==3 || j==5)))) return false 
            if(a[i][j]==-2) if(!((i==9 && (j==3||j==5)) || (i==8 && j==4) || (i==7 && (j==3 || j==5)))) return false 
            if(a[i][j]==3)  if(!((i==0 && (j==2 || j==6)) || (i==2 && (j==0 || j== 4 || j==8)) || (i==4 && (j==2 || j==6)))) return false
            if(a[i][j]==-3) if(!((i==9 && (j==2 || j==6)) || (i==7 && (j==0 || j== 4 || j==8)) || (i==5 && (j==2 || j==6)))) return false
            if(a[i][j]==7)  if(i<3 || (i==3 || i==4) && (j!=0 && j!=2 && j!=4 && j!=6 && j!=8)) return false 
            if(a[i][j]==-7) if(i>6 || (i==6 || i==5) && (j!=0 && j!=2 && j!=4 && j!=6 && j!=8)) return false
        }
    }
    for(var j=0; j<9; j++) if((a[3][j]==7 && a[4][j]==7) || (a[6][j]==-7 && a[5][j]==-7)) return false
    for(var i=0; i<2; i++){
        if(counter[(i*2-1)*1]!=1) return false
        for(var j=2; j<7; j++) if(counter[(i*2-1)*j]>2) return false
        if(counter[(i*2-1)*7]>5) return false
    }
    if(gj==rj){
        v=0 
        for(var k=gi+1; k<ri; k++) v+=Math.abs(a[k][gj]) 
        if(v==0) return false
    }
    return true
}