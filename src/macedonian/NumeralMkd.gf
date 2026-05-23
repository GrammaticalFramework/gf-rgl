concrete NumeralMkd of Numeral = CatMkd [Numeral,Digits,Decimal] ** open Prelude,ResMkd in {

  lin D_0 = mkDig "0" Pl ;
  lin D_1 = mkDig "1" Sg ;
  lin D_2 = mkDig "2" Pl ;
  lin D_3 = mkDig "3" Pl ;
  lin D_4 = mkDig "4" Pl ;
  lin D_5 = mkDig "5" Pl ;
  lin D_6 = mkDig "6" Pl ;
  lin D_7 = mkDig "7" Pl ;
  lin D_8 = mkDig "8" Pl ;
  lin D_9 = mkDig "9" Pl ;
  lincat Dig = {s : Str; n : Number} ;
  lincat Digit = {s : Str; teen : Str; ten : Str; hundred : Str} ;
  lin IDig d = d ** {tail = T1} ;
  lin IFrac ds d = {s = case ds.hasDot of {
                          True => ds.s;
                          False => ds.s ++ BIND ++ ","
                        } ++ BIND ++ d.s;
                    n = Pl; hasDot = True} ;
  lin IIDig d ds = {s = d.s
                          ++ case ds.tail of {
                               T3 => BIND ++ "." ++ BIND;
                               _ => BIND
                             } ++ ds.s;
                    n = Pl; tail = inc ds.tail} ;
  lin NegDecimal ds = {s = "-" ++ BIND ++ ds.s; n = Pl;
                       hasDot = False} ;
  lin PosDecimal ds = ds ** {hasDot = False} ;
  lincat Sub10 = {s : Str; hundred : Str; n : Number} ;
  lincat Sub100 = {s : Str; n : Number} ;
  lincat Sub1000 = {s : Str; n : Number} ;
  lincat Sub1000000 = {s : Str; n : Number} ;
  lincat Sub1000000000 = {s : Str; n : Number} ;
  lincat Sub1000000000000 = {s : Str; n : Number} ;
  oper mkDig : Str -> Number -> {s : Str; n : Number}
             = \s,n -> {s = s; n = n} ;
  oper mkDigit : Str -> Str -> Str -> Str -> {s : Str; teen : Str;
                                              ten : Str; hundred : Str}
               = \s,teen,ten,hundred -> {s = s; teen = teen; ten = ten;
                                         hundred = hundred} ;
  lin n2 = mkDigit "два" "дванаесет" "дваесет" "двесто" ;
  lin n3 = mkDigit "три" "тринаесет" "триесет" "тристо" ;
  lin n4 = mkDigit "четири" "четиринаесет" "четириесет" "четиристо" ;
  lin n5 = mkDigit "пет" "петнаесет" "педесет" "петсто" ;
  lin n6 = mkDigit "шест" "шеснаесет" "шеесет" "шестсто" ;
  lin n7 = mkDigit "седум" "седумнаесет" "седумдесет" "седумсто" ;
  lin n8 = mkDigit "осум" "осумнаесет" "осумдесет" "осумсто" ;
  lin n9 = mkDigit "девет" "деветнаесет" "деведесет" "деветсто" ;
  lin pot0 n = {s = n.s; hundred = n.hundred; n = Pl} ;
  lin pot01 = {s = "еден"; hundred = "еднасто"; n = Sg} ;
  lin pot0as1 n = n ;
  lin pot1 n = {s = n.ten; n = Pl} ;
  lin pot110 = {s = "десет"; n = Pl} ;
  lin pot111 = {s = "единаесет"; n = Pl} ;
  lin pot1to19 d = {s = d.teen; n = Pl} ;
  lin pot1as2 n = n ;
  lin pot1plus d n = {s = d.ten ++ "и" ++ n.s; n = Pl} ;
  lin pot2 n = {s = n.hundred; n = Pl} ;
  lin pot21 = {s = "сто"; n = Pl} ;
  lin pot2plus n m = {s = n.hundred ++ "и" ++ m.s; n = Pl} ;
  lin pot2as3 n = n ;
  lin pot31 = {s = "илјада"; n = Pl} ;
  lin pot3 n = {s = n.s ++ thousand ! n.n; n = Pl} ;
  lin pot3plus n m = {s = n.s ++ thousand ! n.n ++ m.s; n = Pl} ;
  lin pot3decimal d = {s = d.s ++ "илјади"; n = Pl} ;
  lin pot3as4 n = n ;
  lin pot41 = {s = "милион"; n = Sg} ;
  lin pot4 n = {s = n.s ++ million ! n.n; n = Pl} ;
  lin pot4plus n m = {s = n.s ++ million ! n.n ++ m.s; n = Pl} ;
  lin pot4decimal d = {s = d.s ++ "милиони"; n = Pl} ;
  lin pot4as5 n = n ;
  lin pot51 = {s = "милијарда"; n = Sg} ;
  lin pot5 n = {s = n.s ++ billion ! n.n; n = Pl} ;
  lin pot5plus n m = {s = n.s ++ billion ! n.n ++ m.s; n = Pl} ;
  lin pot5decimal d = {s = d.s ++ "милијарди"; n = Pl} ;

  oper
    thousand : Number => Str = table {Sg => "илјада"; Pl => "илјади"} ;
    million : Number => Str = table {Sg => "милион"; Pl => "милиони"} ;
    billion : Number => Str = table {Sg => "милијарда"; Pl => "милијарди"} ;
}
