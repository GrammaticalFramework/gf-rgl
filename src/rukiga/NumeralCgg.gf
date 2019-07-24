--# -path=.:../prelude:../abstract:../common

concrete NumeralCgg of Numeral = CatCgg [Numeral,Digits] **
  open ResCgg, Prelude in {

lincat 
  Digit = { s : Str; unit : { s:Str ; g : Gender; stem : Str}; ten : { s:Str ; g : Gender} };
  Sub10 = { s : Str; unit : { s:Str ; g : Gender; stem : Str}; ten : { s:Str ; g : Gender}; n:Number};
  Sub100     = {s : Str ; g:Gender; n : Number} ;
  Sub1000    = {s : Str ; g:Gender; n : Number} ;
  Sub1000000 = {s : Str ; g:Gender; n : Number} ;


lin num x = {s = \\_,_=> x.s; g=x.g; n=x.n} ; --Numeral = {s : Res.CardOrd => Res.Agreement => Str ; n : Res.Number} ;
lin n2 = mkNum "biri"  "ibiri" ZERO_ZERO "abiri" I_MA True;
lin n3 = mkNum "shatu"  "ishatu" I_ZERO "ashatu"  I_MA True;
lin n4 = mkNum "na" "ina" I_ZERO "ana" I_MA True;
lin n5 = mkNum "taano"  "itaano" I_ZERO "ataano" I_MA True;
lin n6 = mkNum "kaaga"   "mukaaga" MU_MI "nkaaga" N_ZERO False;
lin n7 = mkNum "shanju"  "mushanju" MU_MI "nshanju" N_ZERO False;
lin n8 = mkNum "naana"  "munaana" MU_MI "kinaana" KI_ZERO False;
lin n9 = mkNum "enda"  "mwenda" MU_MI "kyenda" KI_ZERO False;


lin pot01  = {s = []; 
              unit ={s = "emwe";  g = ZERO_ZERO; stem ="mwe"}; 
              ten  = {s = "ikumi" ; g = I_MA};
              ordinal = "kabanza";
              isOrdDifferent = True;
              n = Sg
              };    -- 1
lin pot0 d = d ** {n = Pl} ; -- Sub10 d * 1
lin pot110 = {s = "ikumi" ; g= I_MA; n = Pl}; --10 -Sub100
lin pot111 = {s = "ikumi na emwe" ; g = ZERO_ZERO; n = Pl}; --11
lin pot1to19 d = {s = "ikumi ne" ++ d.unit.s; g=d.unit.g } ** {n = Pl} ; --12-19
lin pot0as1 n =  {s = n.unit.s; g=n.unit.g}  ** {n = n.n} ; --Sub100 -- coercion of 1..9
lin pot1 d = {s = d.ten.s; g = d.ten.g} ** {n = Pl} ;

lin pot1plus d e = {s = d.ten.s ++ "na" ++ e.unit.s; g = ZERO_ZERO; n=Pl };      --Sub100 ;         -- d * 10 + n

lin pot1as2 n = {s = n.s; g = ZERO_ZERO; n=n.n} ;

lin pot2 d = let 
                  numStr = case  d.unit.g of{
                                  MU_MI => d.unit.s;
                                  _     => d.ten.s
                            };
             in {s = "magana" ++ numStr; g = ZERO_ZERO}  ** {n = Pl} ;

lin pot2plus d e = let 
                      unitFigure = case  d.n of{
                                          Sg => "igana";
                                          _  => "magana"
                                };
                      
                      numStr = case  d.unit.g of{
                                        MU_MI => d.unit.s;
                                        _     => d.ten.s
                              };
                   in {s = unitFigure ++ numStr ++ "na" ++ e.s; g = ZERO_ZERO}  ** {n = Pl} ; -- Sub10 -> Sub100 -> Sub1000 ; * 100 + n


lin pot2as3 n = n ;
lin pot3 n = let 
                unitFigure = case  <n.n> of{
                                    <Sg> => "rukumi";
                                      _  => "enkumi" ++ n.s
                          };
              in {s = unitFigure; g = ZERO_ZERO; n=n.n}; -- Sub1000 -> Sub1000000 ;                -- m * 1000

lin pot3plus n m = let 
                      thousand = case  <n.n> of{
                                          <Sg> => "akasirira";
                                          _    => "obusirira" ++ n.s
                          };
                    in { s = thousand  ++ m.s; g = ZERO_ZERO; n = n.n} ;



  lincat 
    Dig = TDigit ;

  lin
    IDig d = d ** {tail = T1} ;

    IIDig d i = {
      s = \\o,agr => d.s ! NCard ! agr  ++ commaIf i.tail ++ i.s ! o ! agr ;
      n = Pl ;
      tail = inc i.tail
    } ;

    D_0 = mkDig "0" ;
    D_1 = mk3Dig "1" "1" Sg ;
    D_2 = mkDig "2" ;
    D_3 = mkDig "3" ;
    D_4 = mkDig "4" ;
    D_5 = mkDig "5" ;
    D_6 = mkDig "6" ;
    D_7 = mkDig "7" ;
    D_8 = mkDig "8" ;
    D_9 = mkDig "9" ;

  oper
    commaIf : DTail -> Str = \t -> case t of {
      T3 => BIND ++ "," ++ BIND ;
      _  => BIND
      } ;

    inc : DTail -> DTail = \t -> case t of {
      T1 => T2 ;
      T2 => T3 ;
      T3 => T1
      };

    mk2Dig : Str -> Str -> TDigit = \c,o -> mk3Dig c o Pl ;
    mkDig : Str -> TDigit = \c -> mk2Dig c c;

    mk3Dig : Str -> Str -> Number -> TDigit = \c,o,n -> {
      s = table {NCard =>\\_=> c ; NOrd => mkOrdinal c } ;
      n = n
      } ;

    TDigit = {
      s : CardOrd =>Agreement => Str;
      n : Number 
    } ;

    mkOrdinal : Str -> Agreement => Str =\c -> \\agr => mkGenPrepWithIVClitic ! agr ++ c;
{-
--1 Numerals

-- This grammar defines numerals from 1 to 999999. 
-- The implementations are adapted from the
-- [numerals library http://www.cs.chalmers.se/~aarne/GF/examples/numerals/] 
-- which defines numerals for 88 languages.
-- The resource grammar implementations add to this inflection (if needed)
-- and ordinal numbers.
--
-- *Note* 1. Number 1 as defined 
-- in the category $Numeral$ here should not be used in the formation of
-- noun phrases, and should therefore be removed. Instead, one should use
-- [Structural Structural.html]$.one_Quant$. This makes the grammar simpler
-- because we can assume that numbers form plural noun phrases.
--
-- *Note* 2. The implementations introduce spaces between
-- parts of a numeral, which is often incorrect - more work on
-- (un)lexing is needed to solve this problem.

abstract Numeral = Cat [Numeral,Digits] ** {

cat 
  Digit ;       -- 2..9
  Sub10 ;       -- 1..9
  Sub100 ;      -- 1..99
  Sub1000 ;     -- 1..999
  Sub1000000 ;  -- 1..999999

data 
  num : Sub1000000 -> Numeral ; -- 123456 [coercion to top category]

  n2, n3, n4, n5, n6, n7, n8, n9 : Digit ;

  pot01 : Sub10 ;                               -- 1
  pot0 : Digit -> Sub10 ;                       -- d * 1
  pot110 : Sub100 ;                             -- 10
  pot111 : Sub100 ;                             -- 11
  pot1to19 : Digit -> Sub100 ;                  -- 10 + d
  pot0as1 : Sub10 -> Sub100 ;                   -- coercion of 1..9
  pot1 : Digit -> Sub100 ;                      -- d * 10
  pot1plus : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
  pot1as2 : Sub100 -> Sub1000 ;                 -- coercion of 1..99
  pot2 : Sub10 -> Sub1000 ;                     -- m * 100
  pot2plus : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
  pot2as3 : Sub1000 -> Sub1000000 ;             -- coercion of 1..999
  pot3 : Sub1000 -> Sub1000000 ;                -- m * 1000
  pot3plus : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n

-- Numerals as sequences of digits have a separate, simpler grammar

cat 
  Dig ;  -- single digit 0..9

data
  IDig  : Dig -> Digits ;       -- 8
  IIDig : Dig -> Digits -> Digits ; -- 876

  D_0, D_1, D_2, D_3, D_4, D_5, D_6, D_7, D_8, D_9 : Dig ;

-}

}
