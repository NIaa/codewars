<https://www.codewars.com/kata/bash-basics-check-for-file-existence/train/shell>  
<b><b>
Using boolean operators is better:
```
[ -z $1 ] && echo "Nothing to find" || [ -e $1 ] && echo "Found $1" || echo "Can't find $1"
```