concrete NumeralSom of Numeral = CatSom [Numeral,Digits] **
  open Prelude, ResSom in {

oper LinDigit : Type = { s : DForm => Str ;
                         n : Number ;
                         even20 : Even20 } ;

oper mk20Ten : Str -> Str -> Str -> Str -> LinDigit = \tri,t,fiche,h ->
  { s = table { Unit   => tri ;
                Teen   => t ;
                Twenty => fiche ;
                Hund   => h + "TODO"} ;
    even20 = Ten ;
    n = Pl } ;

oper mkeven20 : Str -> Str -> Str -> Str -> LinDigit = \se,t,trifichid,h ->
  { s = table { Unit => se ;
                Teen => t ;
                Twenty => trifichid ;
                Hund => h + "TODO" } ;
    even20 = Even ;
    n = Pl } ;

param Even20 = Ten | Even ;
param DForm = Unit | Teen | Twenty | Hund ;

--lincat Numeral = {s : Str} ;
lincat Digit = LinDigit ;
lincat Sub10 = LinDigit ;
lincat Sub100 = {s : Str ; n : Number } ;
lincat Sub1000 = {s : Str ; n : Number ; isHundred : Bool } ;
lincat Sub1000000 = {s : Str ; n : Number } ;


----------------------------------------------------------------------------


--  num : Sub1000000 -> Numeral ;
lin num x0 = lin Numeral x0 ;

lin n2  = mkeven20 "TODO" "TODO" "TODO" "TODO" ;
lin n3  = mk20Ten "TODO" "TODO" "TODO" "TODO";
lin n4  = mkeven20 "TODO" "TODO" "TODO" "TODO";
lin n5  = mk20Ten "TODO" "TODO" "TODO" "TODO";
lin n6  = mkeven20 "TODO" "TODO" "TODO" "TODO" ;
lin n7  = mk20Ten "TODO" "TODO" "TODO" "TODO" ;
lin n8  = mkeven20 "TODO" "TODO" "TODO" "TODO" ;
lin n9  = mk20Ten "TODO" "TODO" "TODO" "TODO" ;

lin pot01  =
  {s = table {Unit => "TODO" ; Hund => "TODO" ; _ => []} ; even20 = Ten ; n = Sg };
lin pot0 d = d ;
lin pot110 = {s = "TODO" ; n = Pl} ;
lin pot111 = {s = variants {"TODO" ; "TODO"} ; n = Pl} ;
lin pot1to19 d = {s = d.s ! Teen ; n = Pl} ;
lin pot0as1 n = {s = n.s ! Unit ; n = n.n} ;
lin pot1 d =
  {s = case d.even20 of {
              Even => d.s ! Twenty ;
              Ten  => glue (d.s ! Twenty) "TODO" } ;
   n = Pl} ;
lin pot1plus d e =
  {s = case d.even20 of {
              Even => d.s ! Twenty ++ "TODO" ++ e.s ! Unit ;
              Ten  => d.s ! Twenty ++ "TODO" ++ e.s ! Teen } ;
   n = Pl} ;

lin pot1as2 n = n ** { isHundred = False } ;
lin pot2 d = {s = d.s ! Hund ; n = Pl ; isHundred = True } ;
lin pot2plus d e =
  { s = d.s ! Hund ++ "TODO" ++ e.s ;
    n = Pl ;
    isHundred = True } ;
lin pot2as3 n = n ;
lin pot3 n =
  {s = table {Sg => [] ; Pl => n.s } ! n.n ++ "TODO" ;
   n = n.n } ;


lin pot3plus n m =
  let ta = if_then_Str m.isHundred [] "TODO" ; --no `ta' between 1000 and 100
  in
    { s = table {Sg => [] ; Pl => n.s } ! n.n ++ "TODO" ++ ta ++ m.s ;
      n = n.n } ;

----------------------------------------------------------------------------

lincat Dig = TDigit ;

oper
  TDigit : Type = { s : CardOrd => Str ; n : Number } ;
  mkDig : Str -> TDigit = \c -> mk2Dig c Pl ;

  mk2Dig : Str -> Number -> TDigit = \c,num ->
   { s = table { NCard => c ;
                 NOrd => c + "TODO" } ;
     n = num } ;



lin D_0 = mkDig "0" ;
lin D_1 = mk2Dig "1" Sg ;
lin D_2 = mkDig "2" ;
lin D_3 = mkDig "3" ;
lin D_4 = mkDig "4" ;
lin D_5 = mkDig "5" ;
lin D_6 = mkDig "6" ;
lin D_7 = mkDig "7" ;
lin D_8 = mkDig "8" ;
lin D_9 = mkDig "9" ;

    -- : Dig -> Digits ;
lin IDig dig = dig ;
    -- : Dig -> Digits -> Digits ;
lin IIDig dig digs = digs ** {s = \\co => glue (dig.s ! co) (digs.s ! co) } ;

}
