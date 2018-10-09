#include<string.h>
#include<stdio.h>
#include<stdlib.h>
char *multi1(const char *big, const int d){
    int l = strlen(big);
    char *ret = (char *)malloc((2+l)*sizeof(char));
    if(d==0) return "0";
    if(d==1) return strcpy(ret, big);
    ret[l+1]='\0';
    int add = 0;
    for(int i=l; i>0; --i){
       int tmp=(big[i-1]-'0')*d;
       ret[i] = (tmp+add)%10 + '0';
       add = (tmp+add)/10;
    }
    ret[0]=add+'0';
    //free(ret);
    return ret;
}
char *add(const char *s1, const char *s2){
    int l1 = strlen(s1);
    int l2 = strlen(s2);
    int l = l1>l2 ? l1 : l2;
    char *ret = (char *)malloc((2+l)*sizeof(char));
    ret[l+1]='\0';
    int add=0;
    for(int i=0; i<l+1; ++i){
        int tmp = add;
        if(i<l1) tmp+=s1[l1-1-i]-'0';
        if(i<l2) tmp+=s2[l2-1-i]-'0';
        ret[l-i]=tmp%10+'0';
        add=(tmp)/10;
    }
    //free(ret);
    return ret;
}
char *nZeros(const char *s, const int n){
    char *ret = (char *)malloc((1+strlen(s)+n)*sizeof(char));
    strcpy(ret, s);
    for(int i=strlen(s); i<strlen(s)+n; ++i) ret[i]='0';
    ret[strlen(s)+n]='\0';
    //free(ret);
    return ret;
}

char *multiply(const char *s1, const char *s2) {
    int l1 = strlen(s1);
    int l2 = strlen(s2);
    char *ret = (char *)malloc((l1+l2+2)*sizeof(char));
    char *tmp = (char *)malloc((l1+l2+2)*sizeof(char));
    ret[0]='0';
    ret[1]='\0';
    for(int i=0; i<l2; ++i){
        strcpy(tmp, multi1(s1, s2[l2-1-i]-'0'));
        strcpy(tmp, nZeros(tmp, i));
        strcpy(ret, add(ret, tmp));
    }
    char *p = ret;
    while(*p == '0') ++p;
    free(tmp);
    //free(ret);    
    if(*p <= '9' && *p >='0') return p;
    else return "0";
}