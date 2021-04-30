#include <stdio.h>
#include <stdlib.h>

#define MAX_GRID 4096

int grid[MAX_GRID][MAX_GRID];

int main()
{
  FILE *fp;
  char str[100];

  if ((fp = fopen("input.txt", "r")) == NULL) {
    fprintf(stderr, "Input file could not be opened\n");
    exit(0);
  }

  int x, y, i, j;

  for (i = 0; i < MAX_GRID; ++i)
    for (j = 0; j < MAX_GRID; ++j)
      grid[i][j] = 0;

  x = y = MAX_GRID / 2;

  int houses = 1;
  grid[x][y] = 1;
  while (fgets(str, 100, fp)) {
    for (i = 0; i < 100; ++i) {
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
  }

  fclose(fp);

  printf("houses: %d\n", houses);
  return 0;
}
