--# -path=.:../abstract:../common:../prelude

concrete SymbolMay of Symbol = CatMay **
  open Prelude, ParadigmsMay, ResMay, (NM=NounMay) in {

lin

  --  : Symb -> PN ;                -- x
  SymbPN i = mkPN i.s ;

  -- : Int -> PN ;                 -- 27
  IntPN i  = mkPN i.s ;

  -- : Float -> PN ;               -- 3.14159
  FloatPN i = mkPN i.s ;

  -- : Card -> PN ;                -- twelve [as proper name]
  NumPN i = mkPN i.s ;

lin
--  CNIntNP cn i = {} ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and y
  CNSymbNP det cn xs =
    let cnSymb = cn ** {heavyMod = cn.heavyMod ++ xs.s}
     in NM.DetCN det cnSymb ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn i = NM.MassNP (cn ** {heavyMod = cn.heavyMod ++ i.s}) ;

  -- : Symb -> S ;
  SymbS sy = sy ;

  -- : Symb -> Card ;
  SymbNum sy = sy ;

  -- : Symb -> Ord ;
  SymbOrd sy = sy ;

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "dan" ; -- TODO check
  ConsSymb = infixSS "," ;


}
