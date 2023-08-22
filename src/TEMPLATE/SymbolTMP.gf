--# -path=.:../abstract:../common:../prelude

concrete SymbolTMP of Symbol = CatTMP **
  open Prelude, ParadigmsTMP, ResTMP, (Noun=NounTMP) in {

lin

  --  : Symb -> PN ;                -- x
  SymbPN i = mkPN_onRuntimeToken i.s ;

  -- : Int -> PN ;                 -- 27
  IntPN i  = mkPN_onRuntimeToken i.s ;

  -- : Float -> PN ;               -- 3.14159
  FloatPN i = mkPN_onRuntimeToken i.s ;

  -- : Card -> PN ;                -- twelve [as proper name]
  NumPN i = mkPN_onRuntimeToken (i.s ! NCard) ;

lin
--  CNIntNP cn i = {} ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and y
  CNSymbNP det cn xs =
    let cnSymb : CN = cn ** {postmod = cn.postmod ++ xs.s}
     in Noun.DetCN det cnSymb ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn i =
    let cnSymb : CN = cn ** {postmod = cn.postmod ++ i.s}
     in Noun.MassNP cnSymb ;

  -- : Symb -> S ;
  SymbS sy = sy ;

  -- : Symb -> Card ;
  SymbNum sy = mkNumeral_onRuntimeToken sy.s ;

  -- : Symb -> Ord ;
  SymbOrd sy = sy ; ---- TODO: nothing added to it. Lincat of Ord is just SS from the beginning.

  oper
    -- To make Card or PN from a runtime argument, cannot use the single + operation.
    -- See https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#unsupported-token-gluing

    mkNumeral_onRuntimeToken : Str -> LinNumeral = \str -> {
      s = table {
        NCard => str ;
        NOrd  => str ++ BIND ++ "th"
        } ;
      n = Pl ; -- NB. probably singular for number 1
      } ;

    mkPN_onRuntimeToken : Str -> LinPN = \str -> {
      s =
       -- table {_ =>  -- If lincat of PN changes so that it's an inflection table, uncomment this
         str
         -- }
         ;
      n = Sg ;
      } ;

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "and" ; -- this comes between the last two ones
  ConsSymb = infixSS "," ;


}
