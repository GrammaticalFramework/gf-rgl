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
  CNNumNP cn card =
    let s : Case => Str = \\c => cn.s ! Sg ! c ++ card.s ! cn.g ! Nom in npForms s s ** {
    clit = s ;
    a = Ag cn.g Sg P3 ; m = Mod cn.g Sg ;
    hasClit = False ; isDrop = False ; isPron = False ;
    } ;

  CNIntNP cn i =
    let s : Case => Str = \\c => cn.s ! Sg ! c ++ i.s in npForms s s ** {
    clit = s ;
    a = Ag cn.g Sg P3 ; m = Mod cn.g Sg ;
    hasClit = False ; isDrop = False ; isPron = False ;
    } ;

  -- as DetCN in NounCze, with the symbols in apposition
  CNSymbNP det cn xs =
    let s : Case => Str = \\c => det.s ! nounGender cn (numSizeNumber det.size) ! c ++ numSizeForm cn.s det.size c ++ xs.s
    in npForms s s ** {
    clit = s ;
    a = numeralAgr (nounGender cn (numSizeNumber det.size)) det P3 ;
    m = numeralModAgr (nounGender cn (numSizeNumber det.size)) det ;
    hasClit = False ; isDrop = False ; isPron = False ;
    } ;

  SymbS sy = sentence False sy.s [] [] ;

  SymbNum sy = invarNumeral sy.s ; -- "n čísel", like numerals from 5 up
  SymbOrd sy = {s = \\g,n,c =>
    glue sy.s ((adjFormsAdjective (mladyAdjForms "-tý")).s ! g ! n ! c)
    } ;

oper
  symbolPN : Str -> PN
    = \s -> lin PN {s = \\_ => s ; g = Neutr} ;

}
