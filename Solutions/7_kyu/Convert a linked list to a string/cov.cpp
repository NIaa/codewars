#include <string>

std::string stringify(Node* list)
{
  std::string ret;
  while(list!=nullptr){
    ret+=std::to_string(list->data)+" -> ";
    list=list->next;
    }
  ret+="nullptr";
  return ret;
}