#include <stdio.h>
int x[10];
int main() {
  int y[10];
  x[4] = 10;
  y[3] = 11;
  int i = 4;
  int j = 3;
  printf("%d %d", x[i], y[j]);
  return 0;
}
