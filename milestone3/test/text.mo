DEF fac(x:nat, a:nat):nat == mul(x,a)
DEF adds(x:nat, y:nat):nat == fac(x, y)
DEF MAIN:nat == IF eq(fac(1,4),5) THEN 10 ELSE 5 FI
