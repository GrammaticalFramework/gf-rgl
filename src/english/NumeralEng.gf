concrete NumeralEng of Numeral = CatEng [Numeral,Digits,Decimal] ** open Prelude, ResEng in {

lincat
  Digit = {s : DForm => CardOrd => Case => Str} ;
  Sub10 = {s : DForm => CardOrd => Case => Str ; n : Number} ;
  Sub100     = {s : CardOrd => Case => Str ; n : Number} ;
  Sub1000          = {s : Bool => CardOrd => Case => Str ; n : Number} ;
  Sub1000000       = {s : Bool => CardOrd => Case => Str ; n : Number} ;
  Sub1000000000    = {s : Bool => CardOrd => Case => Str ; n : Number} ;
  Sub1000000000000 = {s : Bool => CardOrd => Case => Str ; n : Number} ;

lin num x = x ;
lin n2 = let two = mkNum "two"   "twelve"   "twenty" "second" in
         {s = \\f,o => case <f,o> of {
             <teen,NOrd> => regGenitiveS "twelfth" ;
             _ => two.s ! f ! o
             }
         } ;

lin n3 = mkNum "three" "thirteen" "thirty" "third" ;
lin n4 = mkNum "four"  "fourteen" "forty" "fourth" ;
lin n5 = mkNum "five"  "fifteen"  "fifty" "fifth" ;
lin n6 = regNum "six" ;
lin n7 = regNum "seven" ;
lin n8 = mkNum "eight" "eighteen" "eighty" "eighth" ;
lin n9 = mkNum "nine" "nineteen" "ninety" "ninth" ;

lin pot01 = mkNum "one" "eleven" "ten" "first" ** {n = Sg} ;
lin pot0 d = d ** {n = Pl} ;
lin pot0as1 n = {s = n.s ! unit}  ** {n = n.n} ;

lin pot110 = regCardOrd "ten" ** {n = Pl} ;
lin pot111 = regCardOrd "eleven" ** {n = Pl} ;
lin pot1to19 d = {s = d.s ! teen} ** {n = Pl} ;
lin pot1 d = {s = d.s ! ten} ** {n = Pl} ;
lin pot1plus d e = {
   s = \\o,c => d.s ! ten ! NCard ! Nom ++ BIND ++ "-" ++ BIND ++ e.s ! unit ! o ! c ; n = Pl} ;
lin pot1as2 n = {s = \\_ => n.s; n=n.n} ;

lin pot21 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "hundred").s ! o ! c;
      n = Pl
    } ;
lin pot2 d = {s = \\_,o,c => d.s ! unit ! NCard ! Nom ++ mkCard o "hundred" ! c}  ** {n = Pl} ;
lin pot2plus d e = {
  s = \\_,o,c => d.s ! unit ! NCard ! Nom ++ "hundred" ++ "and" ++ e.s ! o ! c ; n = Pl} ;
lin pot2as3 n = n ;

lin pot31 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "thousand").s ! o ! c;
      n = Pl
    } ;
lin pot3 n = {
  s = \\d,o,c => n.s ! d ! NCard ! Nom ++ mkCard o "thousand" ! c ; n = Pl} ;
lin pot3plus n m = {
  s = \\d,o,c => n.s ! d ! NCard ! Nom ++ "thousand" ++ m.s ! False ! o ! c; n = Pl} ;
lin pot3as4 n = n ;
lin pot3decimal f = {
  s = \\d,o,c => f.s ! NCard ! Nom  ++ mkCard o "thousand" ! c ; n = Pl} ;

lin pot41 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "million").s ! o ! c;
      n = Pl
    } ;
lin pot4 n = {
      s = \\d,o,c => n.s ! d ! NCard ! Nom ++ pot41.s ! True ! o ! c ;
      n = Pl
    } ;
lin pot4plus n1 n2 = {
      s = \\d,o,c => n1.s ! d ! NCard ! Nom ++ pot41.s ! True ! NCard ! Nom ++ "and" ++ n2.s ! True ! o ! c;
      n = Pl
    } ;
lin pot4as5 n = n ;
lin pot4decimal f = {
  s = \\d,o,c => f.s ! NCard ! Nom ++ pot41.s ! True ! o ! c ; n = Pl} ;

lin pot51 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "billion").s ! o ! c;
      n = Pl
    } ;
lin pot5 n = {
      s = \\d,o,c => n.s ! d ! NCard ! Nom ++ pot51.s ! True ! o ! c ;
      n = Pl
    } ;
lin pot5plus n1 n2 = {
      s = \\d,o,c => n1.s ! d ! NCard ! Nom ++ pot51.s ! True ! NCard ! Nom ++ "and" ++ n2.s ! True ! o ! c;
      n = Pl
    } ;
lin pot5decimal f = {
  s = \\d,o,c => f.s ! NCard ! Nom ++ pot51.s ! True ! o ! c ; n = Pl} ;

-- numerals as sequences of digits

  lincat
    Dig = TDigit ;

  lin
    IDig d = d ** {tail = T1} ;

    IIDig d i = {
      s = \\o,c => d.s ! NCard ! Nom ++ commaIf i.tail ++ i.s ! o ! c ;
      n = Pl ;
      tail = inc i.tail
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1st" Sg ;
    D_2 = mk2Dig "2" "2nd" ;
    D_3 = mk2Dig "3" "3rd" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

lin PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {s=\\o,c=>"-" ++ BIND ++ d.s ! o ! c;  hasDot=False; n = Pl} ;
    IFrac d i = {
        s=\\o,c=>d.s ! NCard ! Nom ++
                 if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
                 i.s ! o ! c ;
        hasDot=True;
        n = Pl
    } ;

  oper
    commaIf : DTail -> Str = \t -> case t of {
      T3 => BIND ++ "," ++ BIND ;
      _  => BIND
      } ;

    inc : DTail -> DTail = \t -> case t of {
      T1 => T2 ;
      T2 => T3 ;
      T3 => T1
      } ;

    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c (c + "th") ;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard => regGenitiveS c ; NOrd => regGenitiveS o} ;
      n = n
      } ;

    TDigit = {
      n : Number ;
      s : CardOrd => Case => Str
    } ;

}
