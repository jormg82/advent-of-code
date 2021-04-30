#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
  FILE *fp;
  int total = 0, rib = 0;

  if (argc != 2) {
    printf("Usage: %s input_file\n", argv[0]);
  }

  if ((fp = fopen(argv[1], "r")) == NULL) {
    fprintf(stderr, "Input file could not be opened\n");
    exit(0);
  }

  int x, y, z;
  while (fscanf(fp, "%dx%dx%d\n", &x, &y, &z) != EOF) {
    if (x >= y && x >= z) {
      total += 2*x*y + 2*x*z + 3*y*z;
      rib += 2*(y + z) + x*y*z;
    }
    else if (y >= x && y >= z) {
      total += 2*x*y + 3*x*z + 2*y*z;
      rib += 2*(x + z) + x*y*z;
    }
    else {
      total += 3*x*y + 2*x*z + 2*y*z;
      rib += 2*(x + y) + x*y*z;
    }
  }

  fclose(fp);

  printf("total is: %d\n", total);
  printf("total ribbon is: %d\n", rib);

  return 0;
}
