
# cat input.txt | awk -f day07_1.awk

{
  if ($0 ~ /(AND|OR|LSHIFT|RSHIFT)/)
    wires[$5] = $2 " " $1 " " $3;
  else if ($0 ~ /NOT/)
    wires[$4] = $1 " " $2;
  else
    wires[$3] = $1;
}

END {print "a =", wval("a");}


# need local variables
function wval(w, op, val, id)
{
  if (w ~ /[0-9]+/)
    val = w
  else {
    split(wires[w], op);
    id = op[1];

    if (id == "AND")
      val = and(wval(op[2]), wval(op[3]));
    else if (id == "OR")
      val = or(wval(op[2]), wval(op[3]));
    else if (id == "LSHIFT") 
      val = and(0xffff, lshift(wval(op[2]), wval(op[3])));
    else if (id == "RSHIFT")
      val = and(0xffff, rshift(wval(op[2]), wval(op[3])));
    else if (id == "NOT")
      val = xor(wval(op[2]), 0xffff);
    else
      val = wval(id);

    wires[w] = val;
  }

  return val;
}
