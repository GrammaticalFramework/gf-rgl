--# -path=.:../abstract:../common:../prelude

concrete SymbolKor of Symbol = CatKor **
  open Prelude, ResKor, (NK=NounKor), (VK=VerbKor) in {

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

lin
  -- : CN -> Int -> NP
  CNIntNP cn i = NK.MassNP (cn ** {
    s = \\nf => cn.s ! nf ++ i.s}) ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and y
  CNSymbNP det cn xs =
    let cnSymb : CN = cn ** {s = \\nf => cn.s ! nf ++ xs.s}
     in NK.DetCN det cnSymb ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn i = NK.MassNP (cn ** {
    s = \\nf => cn.s ! nf ++ i.s ! cn.c.origin ! Indep}) ;

  -- : Symb -> S ;
  SymbS sy = {s = \\_ => sy.s ; p = Vowel} ;

  -- : Symb -> Card ;
  SymbNum sy = baseNum ** {s = \\_,_ => sy.s} ;

  -- : Symb -> Ord ;
  SymbOrd sy =
    let comp : Comp = VK.CompAdv (lin Adv sy)
     in {s = comp.s ; n = Pl ; p,pNeg = Vowel} ;

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "과" ; -- 와 after vowel. TODO make it a table.
  ConsSymb = infixSS "," ;


}
