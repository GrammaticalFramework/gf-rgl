--# -path=.:../abstract:../common:../prelude

concrete SymbolSom of Symbol = CatSom **
  open Prelude, ParadigmsSom, ResSom, (NS=NounSom) in {

lin

  --  : Symb -> PN ;                -- x
  SymbPN i = mkPN i.s ;

  -- : Int -> PN ;                 -- 27
  IntPN i  = mkPN i.s ;

  -- : Float -> PN ;               -- 3.14159
  FloatPN i = mkPN i.s ;

  -- : Card -> PN ;                -- twelve [as proper name]
  NumPN i = mkPN i.s ;

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

  BaseSymb = infixSS "oo" ;
  ConsSymb = infixSS "," ;


}
