concrete NumeralBel of Numeral = CatBel ** open Prelude in {

lincat
  Digit = {
    s, fem, teen, ten, hundred : Str ;
    form : NumForm
    } ;
  Sub10, Sub100, Sub1000 = {
    s, fem, hundred : Str ;
    form : NumForm
    } ;
  Sub1000000, Sub1000000000, Sub1000000000000 = {s : Str} ;
  Dig = {s : Str} ;

lin
  num n = n ;

  n2 = mkDigit "два" "дзве" "дванаццаць" "дваццаць" "дзвесце" Few ;
  n3 = mkDigit "тры" "тры" "трынаццаць" "трыццаць" "трыста" Few ;
  n4 = mkDigit "чатыры" "чатыры" "чатырнаццаць" "сорак" "чатырыста" Few ;
  n5 = mkDigit "пяць" "пяць" "пятнаццаць" "пяцьдзясят" "пяцьсот" Many ;
  n6 = mkDigit "шэсць" "шэсць" "шаснаццаць" "шэсцьдзясят" "шэсцьсот" Many ;
  n7 = mkDigit "сем" "сем" "сямнаццаць" "семдзесят" "семсот" Many ;
  n8 = mkDigit "восем" "восем" "васямнаццаць" "восемдзесят" "восемсот" Many ;
  n9 = mkDigit "дзевяць" "дзевяць" "дзевятнаццаць" "дзевяноста" "дзевяцьсот" Many ;

  pot01 = {s = "адзін"; fem = "адна"; hundred = "сто"; form = One} ;
  pot0 d = d ;
  pot0as1 n = n ;
  pot110 = mkNum "дзесяць" Many ;
  pot111 = mkNum "адзінаццаць" Many ;
  pot1to19 d = mkNum d.teen Many ;
  pot1 d = mkNum d.ten Many ;
  pot1plus d n = {
    s = d.ten ++ n.s ;
    fem = d.ten ++ n.fem ;
    hundred = [] ;
    form = n.form
    } ;
  pot1as2 n = n ;
  pot21 = mkNum "сто" Many ;
  pot2 n = mkNum n.hundred Many ;
  pot2plus n m = {
    s = n.hundred ++ m.s ;
    fem = n.hundred ++ m.fem ;
    hundred = [] ;
    form = m.form
    } ;
  pot2as3 n = n ;
  pot31 = {s = "тысяча"} ;
  pot3 n = {s = n.fem ++ scaleForm n.form "тысяча" "тысячы" "тысяч"} ;
  pot3plus n m = {s = (pot3 n).s ++ m.s} ;
  pot3as4 n = n ;
  pot3decimal d = {s = d.s ++ "тысяч"} ;
  pot41 = {s = "мільён"} ;
  pot4 n = {s = n.s ++ scaleForm n.form "мільён" "мільёны" "мільёнаў"} ;
  pot4plus n m = {s = (pot4 n).s ++ m.s} ;
  pot4as5 n = n ;
  pot4decimal d = {s = d.s ++ "мільёна"} ;
  pot51 = {s = "мільярд"} ;
  pot5 n = {s = n.s ++ scaleForm n.form "мільярд" "мільярды" "мільярдаў"} ;
  pot5plus n m = {s = (pot5 n).s ++ m.s} ;
  pot5decimal d = {s = d.s ++ "мільярда"} ;

  IDig d = d ;
  IIDig d ds = {s = d.s ++ BIND ++ ds.s} ;

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
  NegDecimal d = {s = "-" ++ BIND ++ d.s} ;
  IFrac d dig = {s = d.s ++ "." ++ BIND ++ dig.s} ;

oper
  mkDigit : (unit, feminine, teen, ten, hundred : Str) -> NumForm -> {
    s, fem, teen, ten, hundred : Str ;
    form : NumForm
    } = \unit,feminine,teen,ten,hundred,form -> {
      s = unit ; fem = feminine ; teen = teen ; ten = ten ;
      hundred = hundred ; form = form
      } ;

  mkNum : Str -> NumForm -> {s, fem, hundred : Str ; form : NumForm} =
    \s,form -> {s = s ; fem = s ; hundred = [] ; form = form} ;

  scaleForm : NumForm -> (one, few, many : Str) -> Str =
    \form,one,few,many -> case form of {
      One => one ;
      Few => few ;
      Many => many
      } ;

param
  NumForm = One | Few | Many ;

}
