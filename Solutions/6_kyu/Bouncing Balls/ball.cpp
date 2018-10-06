int bouncingBall(double h, double bounce, double window) {
    // your code;
    if(bounce>=1||bounce<=0||h<=0||window>=h) return -1;
    int ret = 1;
    while(h*=bounce,h>window) ret+=2;
    return ret;
}