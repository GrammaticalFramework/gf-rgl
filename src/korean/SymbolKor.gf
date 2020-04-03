--# -path=.:../abstract:../common:../prelude

concrete SymbolKor of Symbol = CatKor **
  open Prelude, ResKor in {

lin

  --  : Symb -> PN ;                -- x
  SymbPN i = mkPN i.s ;

  -- : Int -> PN ;                 -- 27
  IntPN i  = mkPN i.s ;

  -- : Float -> PN ;               -- 3.14159
  FloatPN i = mkPN i.s ;

  -- : Card -> PN ;                -- twelve [as proper name]
  NumPN i = mkPN (i.s ! NK ! Indep) ;

oper

  mkPN : Str -> NounPhrase = \s -> {
    s = \\_ => s ;
    p = Consonant ; -- ??
    } ;

{-
lin
--  CNIntNP cn i = {} ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and y
  CNSymbNP det cn xs =
    let cnSymb = cn ** { comp = cn.comp ++ xs.s }
     in NS.DetCN det cnSymb ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn i = NS.MassNP (cn ** { comp = cn.comp ++ i.s }) ;

  -- : Symb -> S ;
  SymbS sy = {s = } ;
  -- : Symb -> Card ;
  SymbNum sy = { s = sy.s ; n = Pl } ;

  -- : Symb -> Ord ;
  SymbOrd sy = { s =} ;
-}
lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "과" ; -- 와 after vowel. TODO make it a table.
  ConsSymb = infixSS "," ;


}
