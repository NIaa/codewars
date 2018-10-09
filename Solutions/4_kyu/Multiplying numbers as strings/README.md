<https://www.codewars.com/kata/multiplying-numbers-as-strings>
<b><b>
```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *multiply(const char *x, const char *y) {
  int i = 0, j = 0, offset = 0;
  char temp = 0, carry = 0;
  size_t xlen = strlen(x), ylen = strlen(y);
  size_t zlen = xlen + ylen;
  char *sx = (char *) calloc(xlen + 1, sizeof(char));
  char *sy = (char *) calloc(ylen + 1, sizeof(char));
  char *sz = (char *) calloc(zlen + 1, sizeof(char));
  for (i = 0; i < xlen; i++)
    sx[i] = x[i] - '0';
  for (i = 0; i < ylen; i++)
    sy[i] = y[i] - '0';
  for (j = ylen - 1; j >= 0; --j) {
    for (i = xlen - 1; i >= 0; --i) {
      temp = sz[i + j + 1] + sx[i] * sy[j];
      sz[i + j + 1] = temp % 10;
      sz[i + j] += temp / 10;
    }
  }
  while (offset < zlen - 1  && sz[offset]==0)
    offset++;
  for (i = 0; i < zlen - offset; i++)
    sz[i] = sz[i+offset] + '0';
  sz[i] = '\0';
  return sz;
}
```