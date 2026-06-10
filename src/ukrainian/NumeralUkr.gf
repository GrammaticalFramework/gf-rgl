concrete NumeralUkr of Numeral = CatUkr [Numeral,Digits,Decimal] ** open Prelude,ParamX,ResUkr in {
  flags
    coding = "UTF-8" ;

lincat
  Digit = {s,teen,ten,hundred : Str} ;
  Sub10 = {s,hundred : Str} ;
  Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 = {s : Str} ;
  Dig = {s : Str} ;

lin
  num n = n ;

  n2 = {s = "два"; teen = "дванадцять"; ten = "двадцять"; hundred = "двісті"} ;
  n3 = {s = "три"; teen = "тринадцять"; ten = "тридцять"; hundred = "триста"} ;
  n4 = {s = "чотири"; teen = "чотирнадцять"; ten = "сорок"; hundred = "чотириста"} ;
  n5 = {s = "п'ять"; teen = "п'ятнадцять"; ten = "п'ятдесят"; hundred = "п'ятсот"} ;
  n6 = {s = "шість"; teen = "шістнадцять"; ten = "шістдесят"; hundred = "шістсот"} ;
  n7 = {s = "сім"; teen = "сімнадцять"; ten = "сімдесят"; hundred = "сімсот"} ;
  n8 = {s = "вісім"; teen = "вісімнадцять"; ten = "вісімдесят"; hundred = "вісімсот"} ;
  n9 = {s = "дев'ять"; teen = "дев'ятнадцять"; ten = "дев'яносто"; hundred = "дев'ятсот"} ;

  pot01 = {s = "один"; hundred = "сто"} ;
  pot0 d = {s = d.s; hundred = d.hundred} ;
  pot0as1 n = n ;
  pot110 = {s = "десять"} ;
  pot111 = {s = "одинадцять"} ;
  pot1to19 d = {s = d.teen} ;
  pot1 d = {s = d.ten} ;
  pot1plus d n = {s = (pot1 d).s ++ n.s} ;
  pot1as2 n = n ;
  pot21 = {s = "сто"} ;
  pot2 n = {s = n.hundred} ;
  pot2plus n m = {s = (pot2 n).s ++ m.s} ;
  pot2as3 n = n ;
  pot31 = {s = "тисяча"} ;
  pot3 n = {s = n.s ++ "тисяч"} ;
  pot3plus n m = {s = (pot3 n).s ++ m.s} ;
  pot3as4 n = n ;
  pot3decimal d = {s = d.s ++ "тисяч"} ;
  pot41 = {s = "мільйон"} ;
  pot4 n = {s = n.s ++ "мільйонів"} ;
  pot4plus n m = {s = (pot4 n).s ++ m.s} ;
  pot4as5 n = n ;
  pot4decimal d = {s = d.s ++ "мільйонів"} ;
  pot51 = {s = "мільярд"} ;
  pot5 n = {s = n.s ++ "мільярдів"} ;
  pot5plus n m = {s = (pot5 n).s ++ m.s} ;
  pot5decimal d = {s = d.s ++ "мільярдів"} ;

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
  IDig d = d ;
  IIDig d ds = {s = d.s ++ BIND ++ ds.s} ;
  PosDecimal ds = ds ;
  NegDecimal ds = {s = "-" ++ BIND ++ ds.s} ;
  IFrac d dig = {s = d.s ++ "." ++ BIND ++ dig.s} ;
}
