#include <stdio.h>
#include <stdlib.h>

#define MAX_GRID 4096
#define MAX_BUF 99

int grid[MAX_GRID][MAX_GRID];

int main()
{
  FILE *fp;
  char str[MAX_BUF];

  if ((fp = fopen("input.txt", "r")) == NULL) {
    fprintf(stderr, "Input file could not be opened\n");
    exit(0);
  }

  int x, y, xr, yr, i, j;

  for (i = 0; i < MAX_GRID; ++i)
    for (j = 0; j < MAX_GRID; ++j)
      grid[i][j] = 0;

  x = y = xr = yr = MAX_GRID / 2;

  int houses = 1;
  grid[x][y] = 1;
  while (fgets(str, MAX_BUF, fp)) {
    for (i = 0; i < MAX_BUF; ++i) {
      if (i % 2) {
        if (str[i] == '^') ++y;
        else if (str[i] == 'v') --y;
        else if (str[i] == '>') ++x;
        else if (str[i] == '<') --x;
        else if (str[i] == '\0') break;
 
        if (grid[x][y] == 0) {
          grid[x][y] = 1;
          ++houses;
        }
      }
      else {
        if (str[i] == '^') ++yr;
        else if (str[i] == 'v') --yr;
        else if (str[i] == '>') ++xr;
        else if (str[i] == '<') --xr;
        else if (str[i] == '\0') break;

        if (grid[xr][yr] == 0) {
          grid[xr][yr] = 1;
          ++houses;
        }
      }
    }
  }

  fclose(fp);

  printf("houses: %d\n", houses);
  return 0;
}
