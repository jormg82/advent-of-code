
# cat input.txt | awk -f day06_2.awk


/on/ {
  split($3, src, ",");
  split($5, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++) {
      grid[i,j]++;
    }
}

/off/ {
  split($3, src, ",");
  split($5, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++)
      grid[i,j]=grid[i,j]>0 ? --grid[i,j] : 0;
}

/toggle/ {
  split($2, src, ",");
  split($4, dst, ",");

  x1=src[1]; y1=src[2];
  x2=dst[1]; y2=dst[2];

  for (i=x1; i<=x2; i++)
    for (j=y1; j<=y2; j++)
      grid[i,j]+=2;
}

END {
  total=0;
  for (val in grid)
    total+=grid[val];

  print "Lights total:", total
}
