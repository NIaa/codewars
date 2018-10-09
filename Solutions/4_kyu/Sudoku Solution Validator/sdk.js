function validSolution(b){
  for(var i=0; i<9; i++){
    buf_row=[0,0,0,0,0,0,0,0,0]
    buf_col=[0,0,0,0,0,0,0,0,0]  
    for(var j=0; j<9; j++) {buf_row[b[i][j]-1]+=1; buf_col[b[j][i]-1]+=1}
    for(var j=0; j<9; j++) if(buf_row[j]!=1 || buf_col[j]!=1) return false
  }
  for(var i=0; i<3; i++){
    for(var j=0; j<3; j++){
      buf_grid=[0,0,0,0,0,0,0,0,0]  
      for(var m=0; m<3; m++) for(var n=0; n<3; n++) buf_grid[b[i*3+m][j*3+n]-1]+=1 
      for(var j=0; j<9; j++) if(buf_grid[j]!=1) return false      
    }
  }
  return true
}