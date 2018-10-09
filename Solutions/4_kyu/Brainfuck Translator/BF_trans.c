#include <stdlib.h>
#include <stdio.h>
#include <string.h>
const char* brainfuck_to_c(const char* source){
    char *s = (char*) malloc(sizeof(char)*strlen(source));        
    if(s==NULL) return "MALLOC ERROR!";

    char *p = s;
    int layer = 0;
    
    for(; *source; ++source){
        const char *src;
        switch(*source){
            case ',': *p++ = *source; break;
            case '.': *p++ = *source; break;
            case '[':  *p++ = *source;  layer += 1;  break;
            case ']': 
                layer--;
                if(layer<0) return "Error!";
                if (*(p-1)=='['){ --p; *p='\0'; }
                else *p++ = *source;
                break;            
            case '+': 
                if(*(p-1)=='-'){ --p; *p='\0'; }
                else *p++ = *source;
                break;
            case '-': 
                if(*(p-1)=='+'){ --p; *p='\0'; }
                else *p++ = *source;
                break;
            case '>':
                if(*(p-1)=='<'){ --p; *p='\0'; }
                else *p++ = *source;
                break;
            case '<': 
                if(*(p-1)=='>'){ --p; *p='\0'; }
                else *p++ = *source;
                break;
            default:;
        }
    }
    *p='\0';
    if(layer) return "Error!";
    
    p = s;
    layer = 0;
    char *ret = (char*) malloc(sizeof(char)*100000000);
    //char *ret = (char*) malloc(sizeof(char)*(2*max_layer+9)*strlen(s));
    if(NULL == s) return "MALLOC ERROR!";
  
    char *p_ret = ret;

    for(; *p; ++p){
        int count = 1;
        for(int i=0; i<layer; ++i) p_ret += sprintf(p_ret, "  ");
        switch(*p){
            case '.': 
                p_ret += sprintf(p_ret, "putchar(*p);\n");
                break;
            case ',': ;
                p_ret += sprintf(p_ret, "*p = getchar();\n");
                break;
            case '+':
                while(*(p+1)=='+'){ ++p; ++count;
                }
                p_ret += sprintf(p_ret, "*p += %d;\n", count);
                break;
            case '-': 
                while(*(p+1)=='-'){ ++p; ++count; }
                p_ret += sprintf(p_ret, "*p -= %d;\n", count);
                break;
            case '>': 
                while(*(p+1)=='>'){ ++p; ++count; }            
                p_ret += sprintf(p_ret, "p += %d;\n", count);
                break;            
            case '<': 
                while(*(p+1)=='<'){ ++p; ++count; }            
                p_ret += sprintf(p_ret, "p -= %d;\n", count);
                break;            
            case '[': 
                ++layer;
                p_ret += sprintf(p_ret, "if (*p) do {\n");
                break;
            case ']': 
                --layer;
                p_ret-=2;
                p_ret += sprintf(p_ret, "} while (*p);\n");
                break;
        }
    }
    free(s);    
    *p_ret='\0';
    return ret;
}