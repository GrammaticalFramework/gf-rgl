concrete NumeralTMP of Numeral = CatTMP [Numeral,Digits] **
  open Prelude, ResTMP in {

  lincat
    Digit = LinNumeral ;      -- 2..9
    Sub10,                    -- 1..9
    Sub100,                   -- 1..99
    Sub1000,                  -- 1..999
    Sub1000000,               -- 1..999999
    Sub1000000000,            -- 1..999999999
    Sub1000000000000          -- 1..999999999999
     = LinNumeral ;

-- param CardOrd   defined in ResTMP
-- type LinNumeral  -""-


  lin
    -- : Sub1000000 -> Numeral ; -- 123456 [coercion to top category]
    num x = x ;

    -- : Digit ;
    n2 = mkNumeral "two" ;
    n3 = mkNumeral "three" ;
    n4 = mkNumeral "four" ;
    n5 = mkNumeral "five" ;
    n6 = mkNumeral "six" ;
    n7 = mkNumeral "seven" ;
    n8 = mkNumeral "eight" ;
    n9 = mkNumeral "nine" ;

    -- : Sub10 ;                            -- 1
    -- pot01 =

    -- : Digit -> Sub10 ;                   -- d * 1
    pot0 d = d ;

    -- : Sub100 ;                           -- 10
    -- pot110 = mkNum "ten" ;

    -- : Sub100 ;                           -- 11
    -- pot111 = mkNum "eleven" ;

    -- : Digit -> Sub100 ;                  -- 10 + d
    -- pot1to19 d =

    -- : Sub10 -> Sub100 ;                  -- coercion of 1..9
    pot0as1 n = n ;

    -- : Digit -> Sub100 ;                  -- d * 10
    -- pot1 d =

    -- : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
    -- pot1plus d e =

    -- : Sub100 -> Sub1000 ;                -- coercion of 1..99
    pot1as2 n = n ;

    -- : Sub10 -> Sub1000 ;                 -- m * 100
    -- pot2 d =

    -- : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
    -- pot2plus d e =

    -- : Sub1000 -> Sub1000000 ;            -- coercion of 1..999
    pot2as3 n = n ;

    -- : Sub1000 -> Sub1000000 ;            -- m * 1000
    -- pot3 d =

    -- : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n
    -- pot3plus d e =

--------------------------------------------------------------------------------
-- Numerals as sequences of digits have a separate, simpler grammar
--

  lincat
    Dig = LinDig ;  -- single digit 0..9

  lin
    -- : Dig -> Digits ;       -- 8
    IDig d = d ;

    -- : Dig -> Digits -> Digits ; -- 876
    IIDig d e = {
      s = table {
        NCard => glue (d.s ! NCard) (e.s ! NCard) ;
        NOrd => glue (d.s ! NCard) (e.s ! NOrd)
        } ;
      n = Pl ;
      } ;

    -- : Dig ;
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
    LinDig : Type = {s : CardOrd => Str ; n : Number} ;
    mkDig : Str -> LinDig = \s -> {
      s = table {
        NCard => s ;
        NOrd  => s + "th"
        } ;
      n = Pl ; -- TODO: handle number 1
      } ;
}
