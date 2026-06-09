concrete NumeralBel of Numeral = CatBel ** {

lincat
  Digit, Sub10, Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 = {s : Str} ;
  Dig = {s : Str} ;

lin
  num n = n ;

  n2 = {s = "2"} ;
  n3 = {s = "3"} ;
  n4 = {s = "4"} ;
  n5 = {s = "5"} ;
  n6 = {s = "6"} ;
  n7 = {s = "7"} ;
  n8 = {s = "8"} ;
  n9 = {s = "9"} ;

  pot01 = {s = "1"} ;
  pot0 d = d ;
  pot0as1 n = n ;
  pot110 = {s = "10"} ;
  pot111 = {s = "11"} ;
  pot1to19 d = {s = "1" ++ d.s} ;
  pot1 d = {s = d.s ++ "0"} ;
  pot1plus d n = {s = d.s ++ n.s} ;
  pot1as2 n = n ;
  pot21 = {s = "100"} ;
  pot2 n = {s = n.s ++ "00"} ;
  pot2plus n m = {s = n.s ++ "00" ++ m.s} ;
  pot2as3 n = n ;
  pot31 = {s = "1000"} ;
  pot3 n = {s = n.s ++ "000"} ;
  pot3plus n m = {s = n.s ++ "000" ++ m.s} ;
  pot3as4 n = n ;
  pot3decimal d = {s = d.s ++ "тысяч"} ;
  pot41 = {s = "1000000"} ;
  pot4 n = {s = n.s ++ "000000"} ;
  pot4plus n m = {s = n.s ++ "000000" ++ m.s} ;
  pot4as5 n = n ;
  pot4decimal d = {s = d.s ++ "мільёна"} ;
  pot51 = {s = "1000000000"} ;
  pot5 n = {s = n.s ++ "000000000"} ;
  pot5plus n m = {s = n.s ++ "000000000" ++ m.s} ;
  pot5decimal d = {s = d.s ++ "мільярда"} ;

  IDig d = d ;
  IIDig d ds = {s = d.s ++ ds.s} ;

  D_0 = {s = "0"} ;
  D_1 = {s = "1"} ;
  D_2 = {s = "2"} ;
  D_3 = {s = "3"} ;
  D_4 = {s = "4"} ;
  D_5 = {s = "5"} ;
  D_6 = {s = "6"} ;
  D_7 = {s = "7"} ;
  D_8 = {s = "8"} ;
  D_9 = {s = "9"} ;

  PosDecimal d = d ;
  NegDecimal d = {s = "-" ++ d.s} ;
  IFrac d dig = {s = d.s ++ "." ++ dig.s} ;

}
