--# -path=.:../abstract:../common:../prelude

concrete SymbolCze of Symbol = CatCze ** open Prelude, ResCze in {

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "a" ;
  ConsSymb = infixSS bindComma ;

  SymbPN s  = symbolPN s.s ;
  IntPN i   = symbolPN i.s ;
  FloatPN f = symbolPN f.s ;

  -- unlike a bare symbol, a cardinal used as a name still declines
  NumPN card = lin PN {s = \\c => card.s ! Neutr ! c ; g = Neutr} ;

  -- the numeral is an invariable label: "úroveň pět", "na úrovni pět"
  CNNumNP cn card = {
    s,clit,prep = \\c => cn.s ! Sg ! c ++ card.s ! cn.g ! Nom ;
    a = Ag cn.g Sg P3 ;
    hasClit = False ;
    } ;

  CNIntNP cn i = {
    s,clit,prep = \\c => cn.s ! Sg ! c ++ i.s ;
    a = Ag cn.g Sg P3 ;
    hasClit = False ;
    } ;

  -- as DetCN in NounCze, with the symbols in apposition
  CNSymbNP det cn xs = {
    s,clit,prep = \\c => det.s ! nounGender cn (numSizeNumber det.size) ! c ++ numSizeForm cn.s det.size c ++ xs.s ;
    a = numSizeAgr (nounGender cn (numSizeNumber det.size)) det.size P3 ;
    hasClit = False ;
    } ;

  SymbS sy = sy ;

  SymbNum sy = {s = \\_,_ => sy.s ; size = Num5} ; -- "n čísel", like numerals from 5 up
  SymbOrd sy = {s = \\g,n,c =>
    glue sy.s ((adjFormsAdjective (mladyAdjForms "-tý")).s ! g ! n ! c)
    } ;

oper
  symbolPN : Str -> PN
    = \s -> lin PN {s = \\_ => s ; g = Neutr} ;

}
