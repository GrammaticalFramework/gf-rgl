concrete NumeralSom of Numeral = CatSom [Numeral,Digits] **
  open Prelude, ResSom, ParamSom in {

oper
  LinDigit : Type = {
    unit : {s : DForm => Str ; ord : Str ; da : DefArticle} ;
    ten : {s, ord : Str ; da : DefArticle}
    } ;

  mkNum3 : (ucard,tcard,uord : Str) -> DefArticle -> LinDigit = \uc,tc,uo,ud -> {
    unit = {s = \\df => uc ; ord = uo ; da = ud} ;
    ten  = {s = tc ; ord = tc + "aad" ; da = M KA}
    } ;

  mkNum2 : (ucard,tcard : Str) -> LinDigit = \uc,tc ->
    let uo : Str = case uc of {
               x + "a" => x + "aad" ; -- ??
               x + #v + c@#c => x + c + "aad" ;
               _             => uc + "aad" } ;
     in mkNum3 uc tc uo (F DA) ;

  mkNum2Masc : (ucard,tcard : Str) -> LinDigit = \uc,tc ->
    let uo : Str = case uc of {
               x + "a" => x + "aad" ; -- ??
               x + #v + c@#c => x + c + "aad" ;
               _             => uc + "aad" } ;
     in mkNum3 uc tc uo (M KA) ;


lincat
  Digit = LinDigit ;

  {- Saeed p. 70-71: "When used with nouns, the cardinal numerals act as
  the head of the construction and the counted noun occurs in the genitive
  case. […] This construction also occurs with composite numerals like
  laba boqol 'two hundred',
  where laba 'two' is a feminine noun in absolutive case and
  boqol 'hundred' is a masculine noun in genitive case.
  Since laba is head, the whole nominal is feminine." -}
  Sub10, Sub100, Sub1000, Sub1000000 = {
    s : DForm => Str ;
    thousand : Str ; -- TODO figure out if this really works so
    hasThousand : Bool ;
    ord : Str ;
    da : DefArticle ;
    n : Number
  } ;

----------------------------------------------------------------------------


--  num : Sub1000000 -> Numeral ;
lin num x = x ;

oper kow : DForm => Str = table {Kow => "kow" ; Hal => "hal" ; Mid => "mid"}  ;
oper n1 : LinDigit = let one : LinDigit = mkNum2 "kow" "toban" in one ** {
    unit = one.unit ** {s = kow}
    } ;
lin n2  = mkNum2 "laba" "labaatan" ;
lin n3  = mkNum2 "saddex" "soddon" ;
lin n4  = mkNum2 "afar" "afartan";
lin n5  = mkNum2 "shan" "konton";
lin n6  = mkNum2 "lix" "lixdan" ;
lin n7  = mkNum2 "toddoba" "toddobaatan" ;
lin n8  = mkNum2Masc "siddeed" "siddeetan" ;
lin n9  = mkNum2Masc "sagaal" "sagaashan" ;

lin pot01 = n1.unit ** {n = Sg ; thousand = [] ; hasThousand = False} ;

lin pot0 d = d.unit ** {n = Pl ; thousand = [] ; hasThousand = False} ;

lin pot110 = n1.ten ** {
    s = \\df => n1.ten.s ;
    thousand = [] ;
    hasThousand = False ;
    n = Pl
    } ;
lin pot111 = {
    s = \\_ => "koob iyo" ++ n1.ten.s ;
    ord = "koob iyo" ++ n1.ten.ord ;
    thousand = [] ;
    hasThousand = False ;
    da = M KA ;
    n = Pl
    } ;
lin pot1to19 d = {
    s = \\_ => d.unit.s ! Hal ++ "iyo" ++ n1.ten.s ;
    thousand = [] ;
    hasThousand = False ;
    ord = d.unit.s ! Hal ++ "iyo" ++ n1.ten.ord ;
    da = M KA ;
    n = Pl
    } ;
lin pot0as1 n = n ;
lin pot1 d = d.ten ** {
    s = \\df => d.ten.s ;
    thousand = [] ;
    hasThousand = False ;
    n = Pl
    } ;
lin pot1plus d e = d.ten ** {
    s = \\b => e.s ! b  ++ "iyo" ++ d.ten.s ;
    ord = e.s ! Hal ++ "iyo" ++ d.ten.ord ;
    thousand = [] ;
    hasThousand = False ;
    n = Pl ;
  } ;
lin pot1as2 n = n ;
lin pot2 d = d ** {
    thousand = "boqol" ;
    hasThousand = True ;
    ord = d.s ! Hal ++ "boqlaad"
    } ;
lin pot2plus d e = d ** {
    thousand = "boqol iyo" ++ e.s ! Hal ;
    hasThousand = True ;
    ord = d.s ! Hal ++ "boqol iyo" ++ e.ord ;
    n = Pl} ;
lin pot2as3 n = n ;
lin pot3 n = n ** {
    thousand = n.thousand ++ "kun" ;
    hasThousand = True ;
    ord = n.s ! Hal ++ "kunaad" ;
    n = Pl } ;

lin pot3plus n m = n ** {
  s = \\b => n.s ! b ++ "kun iyo" ++ m.s ! b ;
  ord = n.ord ++ "kun iyo" ++ m.ord ;
  n = Pl} ;


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
