#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 256

int main()
{
  FILE* fp;
  char str[MAX_LEN];

  if ((fp = fopen("input.txt", "r")) == NULL) {
    fprintf(stderr, "Input file could not be opened\n");
    exit(0);
  }

  int ecars=0, tcars=0;
  while (fgets(str, MAX_LEN-1, fp)) {
    str[strcspn(str, "\n")] = 0;

    int l=strlen(str);
    tcars+=l;
    ecars+=2;
    for (int i=0; i<l; ++i) {
      switch (str[i]) {
      case '"': case '\\': ecars+=2; break;
      default: ecars++;
      }
    }
  }

  fclose(fp);

  printf("Total characters: %d\n", tcars);
  printf("Encoded characters: %d\n", ecars);
  printf("Difference: %d\n", ecars-tcars);

  return 0;
}
