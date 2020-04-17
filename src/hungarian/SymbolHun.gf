concrete SymbolHun of Symbol = CatHun **
  open Prelude, ResHun, (NH=NounHun) in {

lin

  --  : Symb -> PN ;                -- x
  SymbPN i = mkPN i.s ;

  -- : Int -> PN ;                 -- 27
  IntPN i  = mkPN i.s ;

  -- : Float -> PN ;               -- 3.14159
  FloatPN i = mkPN i.s ;

  -- : Card -> PN ;                -- twelve [as proper name]
  NumPN i = mkPN (i.s ! Indep) ;

oper

  mkPN : Str -> NounPhrase = \s -> emptyNP ** {
    s = \\_ => s ;
    } ;

lin
  -- : CN -> Int -> NP
  CNIntNP cn i = NH.MassNP (cn ** {
    s = \\n,c => cn.s ! n ! c ++ i.s}) ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and y
  CNSymbNP det cn xs =
    let cnSymb : CN = cn ** {s = \\n,c => cn.s ! n ! c ++ xs.s}
     in NH.DetCN det cnSymb ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn i = NH.MassNP (cn ** {
    s = \\n,c => cn.s ! n ! c ++ i.s ! Indep}) ;

  -- : Symb -> S ;
  SymbS sy = sy ;

  -- : Symb -> Card ;
  SymbNum sy = baseNum ** {s = \\_ => sy.s} ;

  -- : Symb -> Ord ;
  SymbOrd sy = {s = \\n => sy.s ; n=Pl} ;

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "és" ;
  ConsSymb = infixSS "," ;

}
