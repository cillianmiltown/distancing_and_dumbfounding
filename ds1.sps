SET DECIMAL=DOT.

DATA LIST FILE= "ds1.txt"  free (",")
ENCODING="Locale"
/ InCS (F8.0) SocDes 
  .

VARIABLE LABELS
InCS "InCS" 
 SocDes "SocDes" 
 .

VALUE LABELS
/
InCS 
1 "There is nothing wrong." 
 2 "It's wrong but I can't think of a reason." 
 3 "It's wrong and I can provide a valid reason." 
.
VARIABLE LEVEL SocDes 
 (scale).

EXECUTE.
