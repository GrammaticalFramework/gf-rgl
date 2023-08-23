concrete NumeralZul of Numeral = CatZul [Numeral,Digits,Decimal] ** open Prelude, ResZul in {

-- lincat
--   Digit = {s : DForm => CardOrd => Case => Str} ;
--   Sub10 = {s : DForm => CardOrd => Case => Str ; n : Number} ;
--   Sub100     = {s : CardOrd => Case => Str ; n : Number} ;
--   Sub1000    = {s : Bool => CardOrd => Case => Str ; n : Number} ;
--   Sub1000000 = {s : Bool => CardOrd => Case => Str ; n : Number} ;
--
-- lin num x = x ;
-- lin n2 = let two = mkNum "two"   "twelve"   "twenty" "second" in
--          {s = \\f,o => case <f,o> of {
--              <teen,NOrd> => regGenitiveS "twelfth" ;
--              _ => two.s ! f ! o
--              }
--          } ;
--
-- lin n3 = mkNum "three" "thirteen" "thirty" "third" ;
-- lin n4 = mkNum "four"  "fourteen" "forty" "fourth" ;
-- lin n5 = mkNum "five"  "fifteen"  "fifty" "fifth" ;
-- lin n6 = regNum "six" ;
-- lin n7 = regNum "seven" ;
-- lin n8 = mkNum "eight" "eighteen" "eighty" "eighth" ;
-- lin n9 = mkNum "nine" "nineteen" "ninety" "ninth" ;
--
-- lin pot01 = mkNum "one" "eleven" "ten" "first" ** {n = Sg} ;
-- lin pot0 d = d ** {n = Pl} ;
-- lin pot110 = regCardOrd "ten" ** {n = Pl} ;
-- lin pot111 = regCardOrd "eleven" ** {n = Pl} ;
-- lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
-- lin pot0as1 n = {s = n.s ! unit}  ** {n = n.n} ;
-- lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
-- lin pot1plus d e = {
--    s = \\o,c => d.s ! ten ! NCard ! Nom ++ BIND ++ "-" ++ BIND ++ e.s ! unit ! o ! c ; n = Pl} ;
-- lin pot1as2 n = {s = \\_ => n.s; n=n.n} ;
-- lin pot2 d = {s = \\_,o,c => d.s ! unit ! NCard ! Nom ++ mkCard o "hundred" ! c}  ** {n = Pl} ;
-- lin pot2plus d e = {
--   s = \\_,o,c => d.s ! unit ! NCard ! Nom ++ "hundred" ++ "and" ++ e.s ! o ! c ; n = Pl} ;
-- lin pot2as3 n = n ;
-- lin pot3 n = {
--   s = \\d,o,c => n.s ! d ! NCard ! Nom ++ mkCard o "thousand" ! c ; n = Pl} ;
-- lin pot3plus n m = {
--   s = \\d,o,c => n.s ! d ! NCard ! Nom ++ "thousand" ++ m.s ! False ! o ! c; n = Pl} ;
--
-- numerals as sequences of digits
lincat Dig = {s:Str} ;

lin
  IDig d = d ;

  IIDig d dd = {s = d.s ++ Predef.BIND ++ dd.s} ;

  D_0 = { s = "0"} ;
  D_1 = { s = "1"} ;
  D_2 = { s = "2"} ;
  D_3 = { s = "3"} ;
  D_4 = { s = "4"} ;
  D_5 = { s = "5"} ;
  D_6 = { s = "6"} ;
  D_7 = { s = "7"} ;
  D_8 = { s = "8"} ;
  D_9 = { s = "9"} ;

  PosDecimal d = d ** {hasDot=False} ;
  NegDecimal d = {
    s = "-" ++ Predef.BIND ++ d.s ;
    hasDot=False
    } ;
  IFrac d i = {
    s = d.s ++
        if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
        i.s ;
    hasDot=True
    } ;

}
