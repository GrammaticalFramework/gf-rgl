--# -path=.:../abstract:../common:../prelude

concrete SymbolPes of Symbol = CatPes ** open Prelude, ResPes in {

  flags coding = utf8;

 lin
-- SymbPN i = {s = \\_ => i.s ; g = Masc} ;
  SymbPN i = {s = i.s ; animacy = Inanimate} ; -- "از" is removed for Phrasebook
  IntPN i  = {s = i.s ; animacy = Inanimate} ;
  FloatPN i = {s = i.s ; animacy = Inanimate} ;
  NumPN i = {s = i.s ; animacy = Inanimate} ;
  CNIntNP cn i = emptyNP ** cn ** {
    s = \\ez => cn.s ! Sg ! Ezafe ++ i.s ++ cn.compl ! Sg ;
    a = agrP3 Sg
    } ;
  CNSymbNP det cn xs = emptyNP ** cn ** {
    s = \\ez => det.s ++ cn.s ! det.n ! Ezafe ++ xs.s ++ cn.compl ! det.n ;
    a = agrP3 det.n
    } ;
  CNNumNP cn i = emptyNP ** cn ** {
    s = \\ez => cn.s ! Sg ! Ezafe ++ i.s ++ cn.compl ! Sg ;
    a = agrP3 Sg ;
    } ;

  SymbS sy = {s = \\_ => sy.s} ;
  SymbNum sy = {s = sy.s ; n = Pl} ;
  SymbOrd sy = {s = sy.s ; n = Sg ; isNum,isPre=False} ;

lincat

  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "تE" ;
  ConsSymb = infixSS "" ;

  -- TODO: what are wN and تE? /IL

}
