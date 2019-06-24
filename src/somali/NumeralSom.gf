concrete NumeralSom of Numeral = CatSom [Numeral,Digits] **
  open Prelude, ResSom, ParamSom in {

oper
  LinDigit : Type = {
    s : DForm => CardOrd => State => Str -- TODO: for 1, hal and mid. variation kow-koob implemented with pre.
    } ;

  mkNum3 : (ucard,tcard,uord : Str) -> Gender -> LinDigit = \uc,tc,uo,g -> {s =
    \\df,co,s => case <co,df,s> of {
        <NOrd,Unit,_> => uo ;
        <NOrd,Ten, _> => tc + "aad" ;
        <NCard,Unit,s> => nf2state (mkNg uc g) ! s ;
        <NCard,Ten, s> => nf2state (mkN1 tc) ! s }
    } ;

  mkNum2 : (ucard,tcard : Str) -> LinDigit = \uc,tc ->
    let uo : Str = case uc of {
               x + "a" => x + "aad" ; -- ??
               x + #v + c@#c => x + c + "aad" ;
               _             => uc + "aad" } ;
     in mkNum3 uc tc uo Fem ;

  mkNum2Masc : (ucard,tcard : Str) -> LinDigit = \uc,tc ->
    let uo : Str = case uc of {
               x + "a" => x + "aad" ; -- ??
               x + #v + c@#c => x + c + "aad" ;
               _             => uc + "aad" } ;
     in mkNum3 uc tc uo Masc ;


lincat
  Digit = LinDigit ;
  Sub10, Sub100, Sub1000, Sub1000000 =
          {s : CardOrd => State => Str ; n : Number} ;

----------------------------------------------------------------------------


--  num : Sub1000000 -> Numeral ;
lin num x = x ;

oper kow : Str = "kow" ; --pre {"iyo" => "koob" ; _ => "kow"} ;

oper n1 = mkNum3 kow "toban" "kowaad" Fem ;
lin n2  = mkNum2 "laba" "labaatan" ;
lin n3  = mkNum2 "saddex" "soddon" ;
lin n4  = mkNum2 "afar" "afartan";
lin n5  = mkNum2 "shan" "konton";
lin n6  = mkNum2 "lix" "lixdan" ;
lin n7  = mkNum2 "toddoba" "toddobaatan" ;
lin n8  = mkNum2Masc "siddeed" "siddeetan" ;
lin n9  = mkNum2Masc "sagaal" "sagaaashan" ;

lin pot01  = {s = n1.s ! Unit ; n = Sg} ;

lin pot0 d = {s = d.s ! Unit ; n = Pl} ;

lin pot110 = {s = n1.s ! Ten ; n = Pl} ;
lin pot111 = {s = \\co,s => "koob iyo" ++ n1.s ! Ten ! co ! s ; n = Pl} ;
lin pot1to19 d = {s = \\co,s => d.s ! Unit ! co ! s ++ n1.s ! Ten ! co ! s ; n=Pl} ;
lin pot0as1 n = n ;
lin pot1 d = {s = d.s ! Ten ; n=Pl};
  -- {s = d.s ! Unit ;
  --  n = Pl} ;
lin pot1plus d e = {
  s = \\co,s => e.s ! co ! Indefinite ++ "iyo" ++ d.s ! Ten ! co ! s  ;
  n = Pl} ;

lin pot1as2 n = n ;
lin pot2 d = d ** {s = \\co,s => d.s ! co ! s ++ "boqol"} ; -- TODO
lin pot2plus d e = {
  s = \\co,s => d.s ! co ! Indefinite ++ "boqol iyo" ++ e.s ! co ! s ;
  n = Pl} ;
lin pot2as3 n = n ;
lin pot3 n = n ;

lin pot3plus n m = {
  s = \\co,s => n.s ! co ! s ++ "iyo" ++ m.s ! co ! s ;
  n = n.n } ;


--TODO:
-- my three cats
-- * saddexd &+ ayg &+ a bisadood
-- => saddexd &+ ayd &+ a bisadood
-- my *two* thousand small cats
-- => laba kun oo bisadood oo yar (kun is an attribute, bisadood is an attribute)
----------------------------------------------------------------------------

lincat Dig = TDigit ;

oper
  TDigit : Type = { s : CardOrd => Str ; n : Number } ;
  mkDig : Str -> TDigit = \c -> mk2Dig c Pl ;

  mk2Dig : Str -> Number -> TDigit = \c,num ->
   { s = table { NCard => c ;
                 NOrd  => c + "aan" } ;
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
