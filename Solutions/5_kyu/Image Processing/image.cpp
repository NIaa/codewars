#include <vector>
#include <iostream>
using namespace std;

typedef unsigned char u8;

vector <u8> processImage (const vector <u8> &imageData, int height, int width, vector <vector <float>> weights) {
  vector<u8> ret;
  int mat[height][width][3];
  for(int i=0; i<height; i++){
    for(int j=0; j<width; j++){
      mat[i][j][0] = (int)imageData[width*i*3+j*3];
      mat[i][j][1] = (int)imageData[width*i*3+j*3+1];
      mat[i][j][2] = (int)imageData[width*i*3+j*3+2];
    }
  }

  float after[height][width][3];
  int sz=weights.size();
  for(int m=0; m<height; m++){
    for(int n=0; n<width; n++){
      float r=0,g=0,b=0;
      for(int i=0; i<sz; i++){
        for(int j=0; j<sz; j++){
          if(m-sz/2+i<0){
            if(n-sz/2+j<0){
              r+=mat[0][0][0]*weights[i][j];
              g+=mat[0][0][1]*weights[i][j];
              b+=mat[0][0][2]*weights[i][j];
            }
            else if(n-sz/2+j>=width){
              r+=mat[0][width-1][0]*weights[i][j];
              g+=mat[0][width-1][1]*weights[i][j];
              b+=mat[0][width-1][2]*weights[i][j];
            }
            else{
              r+=mat[0][n-sz/2+j][0]*weights[i][j];
              g+=mat[0][n-sz/2+j][1]*weights[i][j];
              b+=mat[0][n-sz/2+j][2]*weights[i][j];
            }
          }
          else if(m-sz/2+i>=height){
            if(n-sz/2+j<0){
              r+=mat[height-1][0][0]*weights[i][j];
              g+=mat[height-1][0][1]*weights[i][j];
              b+=mat[height-1][0][2]*weights[i][j];
            }
            else if(n-sz/2+j>=width){
              r+=mat[height-1][width-1][0]*weights[i][j];
              g+=mat[height-1][width-1][1]*weights[i][j];
              b+=mat[height-1][width-1][2]*weights[i][j];                                        
            }
            else{
              r+=mat[height-1][n-sz/2+j][0]*weights[i][j];
              g+=mat[height-1][n-sz/2+j][1]*weights[i][j];
              b+=mat[height-1][n-sz/2+j][2]*weights[i][j];
            }                
          }
          else{
            if(n-sz/2+j<0){                
              r+=mat[m-sz/2+i][0][0]*weights[i][j];
              g+=mat[m-sz/2+i][0][1]*weights[i][j];
              b+=mat[m-sz/2+i][0][2]*weights[i][j];
            }
            else if(n-sz/2+j>=width){
              r+=mat[m-sz/2+i][width-1][0]*weights[i][j];
              g+=mat[m-sz/2+i][width-1][1]*weights[i][j];
              b+=mat[m-sz/2+i][width-1][2]*weights[i][j];                                        
            }
            else{
              r+=mat[m-sz/2+i][n-sz/2+j][0]*weights[i][j];
              g+=mat[m-sz/2+i][n-sz/2+j][1]*weights[i][j];
              b+=mat[m-sz/2+i][n-sz/2+j][2]*weights[i][j];
            }                                
          }
        }                    
      }
    r+=0.5;    if(r<0) r=0;    if(r>=256) r=255;    after[m][n][0]=r;
    g+=0.5;    if(g<0) g=0;    if(g>=256) g=255;    after[m][n][1]=g;
    b+=0.5;    if(b<0) b=0;    if(b>=255) b=255;    after[m][n][2]=b;
    } 
  }
    for(int i=0; i<height; i++){
        for(int j=0; j<width; j++){
            for(int k=0; k<3; k++){
                ret.push_back((u8)(after[i][j][k]));
            }
        }
    }
    return ret;
}