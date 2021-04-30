
# cat input.txt | awk -f day02_1.awk


BEGIN {FS="x"; total=0;}

{
  x = $1; y = $2; z = $3;

  if (x >= y && x >= z) {
    total += 2*x*y + 2*x*z + 3*y*z;
  }
  else if (y >= x && y >= z) {
    total += 2*x*y + 3*x*z + 2*y*z;
  }
  else {
    total += 3*x*y + 2*x*z + 2*y*z;
  }
}

END {print total;}
