-- David Wahlstedt 2002 (cardinal numbers)
-- Inari Listenmaa 2020 (ordinals + cosmetic changes)
concrete NumeralTam of Numeral = CatTam [Numeral,Digits] **
  open Prelude, ResTam in {

--  lincat
--    Digit = OrdNum ;          -- 2..9
--    Sub10,                    -- 1..9
--    Sub100,                   -- 1..99
--    Sub1000 = LinNumber ;     -- 1..999
--    Sub1000000 = OrdNum ;     -- 1..999999

--  oper
--    LinNumber : Type = {
--      s : DForm => Str ;
--      n : Number ; -- This is an internal number that tells which form of digits to choose. When quantifying a noun, the noun is in singular.
--      ord : Str ;
--      } ;

--    OrdNum : Type = CardOrdNum ** {n : Number} ;

--  lin
--    -- : Sub1000000 -> Numeral ; -- 123456 [coercion to top category]
--    num x = x ;

--    -- : Digit ;
--    n2 = mkDigit "dua" ;
--    n3 = mkDigit "tiga" ;
--    n4 = mkDigit "empat" ;
--    n5 = mkDigit "lima" ;
--    n6 = mkDigit "enam" ;
--    n7 = mkDigit "tujuh" ;
--    n8 = mkDigit "lapan" ; -- "delapan" for Indonesian
--    n9 = mkDigit "sembilan" ;

--    -- : Sub10 ;                            -- 1
--    pot01 = {
--      s = table {
--            Attrib => [] ;
--            Indep => "satu" } ;
--      n = Sg ;
--      ord = "pertama"
--      } ;

--    -- : Digit -> Sub10 ;                   -- d * 1
--    pot0 d = d ** {s = \\_ => d.s} ;

--    -- : Sub100 ;                           -- 10
--    pot110 = mkNum "sepuluh" ;

--    -- : Sub100 ;                           -- 11
--    pot111 = mkNum "sebelas" ;

--    -- : Digit -> Sub100 ;                  -- 10 + d
--    pot1to19 d = mkNum3 d "belas" [] ;

--    -- : Sub10 -> Sub100 ;                  -- coercion of 1..9
--    pot0as1 n = n ;

--    -- : Digit -> Sub100 ;                  -- d * 10
--    pot1 d = mkNum3 d "puluh" [] ;

--    -- : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
--    pot1plus d e = -- 21 = dua puluh satu, so we choose Indep form of 1.
--      mkNum3 d "puluh" (e.s ! Indep) ;

--    -- : Sub100 -> Sub1000 ;                -- coercion of 1..99
--    pot1as2 n = n ;

--    -- : Sub10 -> Sub1000 ;                 -- m * 100
--    pot2 d = potNum d ratus [] ;

--    -- : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
--    pot2plus d e = potNum d ratus (e.s ! Indep) ;

--    -- : Sub1000 -> Sub1000000 ;            -- coercion of 1..999
--    pot2as3 n = n ** {s = n.s ! Indep} ;

--    -- : Sub1000 -> Sub1000000 ;            -- m * 1000
--    pot3 d = pot2as3 (potNum d ribu []) ;

--    -- : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n
--    pot3plus d e = pot2as3 (potNum d ribu (e.s ! Indep)) ;

--oper
--  ratus : Number*CardOrd => Str = table {                 -- 100
--    <Sg, NCard> => "seratus" ;
--    <Sg, NOrd> => "keseratus" ;
--    <Pl,_> => "ratus"
--    } ;

--  ribu : Number*CardOrd => Str = table {                  -- 1000
--    <Sg, NCard> => "seribu" ;
--    <Sg, NOrd> => "keseribu" ;
--    <Pl,_> => "ribu"
--    } ;

--  -- To make Sub* funs directly from a string.
--  -- ordnumeral from here
--  mkNum : Str -> LinNumber = \s -> {
--    n = Pl ;
--    s = \\_ => s ; -- Indep vs. Attrib only matters for number 1
--    ord = "ke" + s ; -- Works for all but number 1
--    } ;

--  mkDigit : Str -> OrdNum = \s -> mkNum s ** {s=s} ;

--  -- Only for Digit -> Sub*: we won't run into 1 here.
--  mkNum3 : (digit : OrdNum) -> (ten,unit : Str) -> LinNumber = \tiga,puluh,dua -> {
--    n = Pl ;
--    s = \\_ => tiga.s ++ puluh ++ dua ;
--    ord = tiga.ord ++ puluh ++ dua
--    } ;

--  -- The most general oper for making new numbers out of old ones.
--  potNum : LinNumber -> (Number*CardOrd => Str) -> Str -> LinNumber = \satu,ribuTbl,dua -> {
--    n = Pl ;
--    s = \\_ =>
--      satu.s ! Attrib ++ -- Attrib form is empty string in 1, and normal for others.
--      ribuTbl ! <satu.n, NCard> ++ dua ;
--    ord = case satu.n of {
--      Sg => satu.s ! Attrib ++ ribuTbl ! <Sg, NOrd>  ++ dua ;
--      Pl => satu.ord      ++ ribuTbl ! <Pl, NCard> ++ dua }
--    } ;

--  -- Numerals as sequences of digits have a separate, simpler grammar
--  lincat
--    Dig = DigNum ;  -- single digit 0..9

--  lin
--    -- : Dig -> Digits ;       -- 8
--    IDig d = d ;

--    -- : Dig -> Digits -> Digits ; -- 876
--    IIDig d e = {
--      s = table {
--        NCard => glue (d.s ! NCard) (e.s ! NCard) ;
--        NOrd => glue (d.s ! NOrd) (e.s ! NCard)
--        }
--      } ;

--    -- : Dig ;
--    D_0 = mkDig "0" ;
--    D_1 = mkDig "1" ;
--    D_2 = mkDig "2" ;
--    D_3 = mkDig "3" ;
--    D_4 = mkDig "4" ;
--    D_5 = mkDig "5" ;
--    D_6 = mkDig "6" ;
--    D_7 = mkDig "7" ;
--    D_8 = mkDig "8" ;
--    D_9 = mkDig "9" ;

--  oper
--    mkDig : Str -> DigNum = \s -> {
--      s = table {
--        NCard => s ;
--        NOrd => "ke-" + s
--        }
--      } ;
--}
}
