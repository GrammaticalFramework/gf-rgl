concrete NumeralHun of Numeral = CatHun [Numeral,Digits] **
  open Prelude, ResHun in {

lincat
  Digit,
  Sub10 = LinDigit ;
  Sub100,
  Sub1000,
  Sub1000000 = ResHun.Numeral ;
lin
  -- TODO: Add case inflection and ordinal forms to all numerals

  -- : Sub1000000 -> Numeral
  num x = x ;

  -- : Digit
  n2 = mkNum5 "kettő" "húsz" "két" "huszon" "második" ;
  n3 = mkNum3 "három" "harminc" "harmadik" ;
  n4 = mkNum3 "négy" "negyven" "negyedik" ;
  n5 = mkNum3 "öt" "ötven" "ötödik" ;
  n6 = mkNum3 "hat" "hatvan" "hatodik" ;
  n7 = mkNum3 "hét" "hetven" "hetedik" ;
  n8 = mkNum3 "nyolc" "nyolcvan" "nyolcadik" ;
  n9 = mkNum3 "kilenc" "kilencven" "kilencedik" ;

  -- : Sub10 ;                               -- 1
  pot01 = mkNum3 "egy" "tíz" "első" ** {n=Sg} ;
  -- : Digit -> Sub10 ;                      -- d * 1
  pot0 d = d ;

  -- : Sub100 ;                              -- 10
  pot110 = {s = table {p => "tíz"} ; n = numNumber ; numtype = IsNum} ;
  -- : Sub100 ;                              -- 11
  pot111 = {s = table {p => "tizenegy"} ; n = numNumber ; numtype = IsNum} ;
  -- : Digit -> Sub100 ;                     -- 10 + d
  pot1to19 d =
      {s = table {p => "tizen" ++ d.s ! <Unit,p>} ;
       n = numNumber ; numtype = IsNum} ;
  --  : Sub10 -> Sub100 ;                    -- coercion of 1..9
  pot0as1 n =
      {s = table {p => n.s ! <Unit,p>} ;
       n = numNumber ; numtype = IsNum} ;

  -- : Digit -> Sub100 ;                     -- d * 10
  pot1 d =
      {s = table {p => d.s ! <Ten,p>} ;
       n = numNumber ; numtype = IsNum} ;
  -- : Digit -> Sub10 -> Sub100 ;            -- d * 10 + n
  pot1plus d e =
      {s = table {p => (d.s ! <Ten,Attrib>) ++ e.s ! <Unit,p>} ;
       n = numNumber ; numtype = IsNum} ;

  -- : Sub100 -> Sub1000 ;                   -- coercion of 1..99
  pot1as2 n = n ;
  --  : Sub10 -> Sub1000 ;                   -- m * 100
  pot2 d =
      {s = table {p => (d.s ! <Unit,Attrib>) ++ "száz"} ;
       n = numNumber ; numtype = IsNum} ;
  --  : Sub10 -> Sub100 -> Sub1000 ;         -- m * 100 + n
  pot2plus d e =
      {s = table {p => (d.s ! <Unit,Attrib>) ++ "száz" ++ e.s ! p} ;
       n = numNumber ; numtype = IsNum} ;

  -- : Sub1000 -> Sub1000000 ;               -- coercion of 1..999
  pot2as3 n = n ;
  -- : Sub1000 -> Sub1000000 ;               -- m * 1000
  pot3 n =
      {s = table {p => n.s ! Attrib ++ "ezer"} ;
       n = numNumber ; numtype = IsNum} ;
  --  : Sub1000 -> Sub1000 -> Sub1000000 ;   -- m * 1000 + n
  pot3plus n m =
      {s = table {p => n.s ! Attrib ++ "ezer" ++ m.s ! p} ;
       n = numNumber ; numtype = IsNum} ;

oper
  LinDigit : Type = {s : DForm*Place => Str ; n : Number} ;

  mkNum3 : (x1,_,x3 : Str) -> LinDigit = \három,harminc,harmadik ->
    mkNum5 három harminc három harminc harmadik ;

  mkNum5 : (x1,_,_,_,x5 : Str) -> LinDigit = \ui,ti,ua,ta,ord -> {
    s = table {<Unit,Indep> => ui ;
               <Ten,Indep> => ti ;
               <Unit,Attrib> => ua ;
               <Ten, Attrib> => ta } ;
--    ord = ord ; -- TODO figure out where to use ordinal
    n = numNumber ;
    numType = IsNum ;
    } ;

   numNumber = Sg ;

  -- numerals as sequences of digits
  lincat
    Dig = TDigit ;

  lin
    -- : Dig -> Digits ;       -- 8
    IDig d = d ** {s = \\_ => d.s} ;

    -- : Dig -> Digits -> Digits ; -- 876
    IIDig d i = {
      s = \\x => d.s ++ BIND ++ i.s ! x ;
      n = numNumber
    } ;

    D_0 = mkDig "0" ;
    D_1 = mkDig "1" ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

  oper
    mkDig : Str -> TDigit = \s -> {
      s = s ;
      n = numNumber
      } ;

    TDigit = {
      s : Str ; -- TODO add ordinals
      n : Number
      } ;

}
