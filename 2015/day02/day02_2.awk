
# cat input.txt | awk -f day02_2.awk


BEGIN {FS="x"; rib=0;}

{
  x = $1; y = $2; z = $3;

  if (x >= y && x >= z) {
    rib += 2*(y + z) + x*y*z;
  }
  else if (y >= x && y >= z) {
    rib += 2*(x + z) + x*y*z;
  }
  else {
    rib += 2*(x + y) + x*y*z;
  }
}

END {print rib;}
