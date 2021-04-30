#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_LEN 256

int main(void)
{
  FILE* fp;
  char str[MAX_LEN];

  if ((fp = fopen("input.txt", "r")) == NULL) {
    fprintf(stderr, "Input file could not be opened\n");
    exit(0);
  }

  int scars=0, tcars=0;
  while (fgets(str, MAX_LEN-1, fp)) {
    str[strcspn(str, "\n")] = 0;

    int l=strlen(str);
    tcars+=l;
    for (int i=1; i<l-1; ++i) {
      if (str[i] == '\\') {
        if (str[i+1]=='"' || str[i+1]=='\\') i++;
        else if (str[i+1]=='x') i+=3;
      }

      scars++;
    }
  }

  fclose(fp);

  printf("Total characters: %d\n", tcars);
  printf("Scaped characters: %d\n", scars);
  printf("Diference: %d\n", tcars-scars);

  return 0;
}
