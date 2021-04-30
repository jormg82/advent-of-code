
# cat input.txt | awk -f day06_1.awk


/on/ {
  split($3, src, ",");
  split($5, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++) {
      grid[i,j]=1;
    }
}

/off/ {
  split($3, src, ",");
  split($5, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++)
      grid[i,j]=0;
}

/toggle/ {
  split($2, src, ",");
  split($4, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++)
      grid[i,j]=!grid[i,j];
}

END {
  total=0;
  for (val in grid)
    total+=grid[val];

  print "Lights on:", total
}
