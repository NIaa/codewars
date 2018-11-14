#include<stdio.h>
#include <stdlib.h>

#define MAX_RETURN_SIZE 1337
#define MAX_INIT_SIZE 30000 //accoding to wiki
char* brain_luck(char* code, char* input)
{
 	char *ret = (char *)calloc(MAX_RETURN_SIZE, 1); //store return
	char *init = (char *)calloc(MAX_INIT_SIZE, 1);  //initial array
	char *r = ret;	
	char *p = init;
    for(; *code; ++code){
		switch(*code){
			case '>': ++p; 			 break;
			case '<': --p; 			 break;
			case '+': ++*p; 		 break;
			case '-': --*p; 		 break;
			case '.': *r++ = *p;	 break;
			case ',': *p = *input++; break;
			case '[':
				if(!*p){
					int layer = 1; //count layer of brackets
					while(layer){
						++code;
						if(*code == '[') ++layer;
						if(*code == ']') --layer;
					}
				}
				break;
			case ']':
				if(*p){
					int layer = 1;
					while(layer){
						--code;
						if(*code == ']') ++layer;
						if(*code == '[') --layer;
					}
				}
				break;
			default: break;
		}			
  	}
	free(init);
	return ret;
}