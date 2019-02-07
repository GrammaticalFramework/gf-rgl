--# -path=.:../abstract:../common:../prelude

concrete SymbolPes of Symbol = CatPes ** open Prelude, ResPes in {

  flags coding = utf8;

 lin
-- SymbPN i = {s = \\_ => i.s ; g = Masc} ;
  SymbPN i = {s = i.s ; animacy = Inanimate} ; -- "از" is removed for Phrasebook
  IntPN i  = {s = i.s ; animacy = Inanimate} ;
  FloatPN i = {s = i.s ; animacy = Inanimate} ;
  NumPN i = {s = i.s ; animacy = Inanimate} ;
  CNIntNP cn i = cn ** {
    s = \\ez => cn.s ! aEzafa ! Sg  ++ i.s ;
    a = agrP3 Sg
    } ;
  CNSymbNP det cn xs = cn ** {
    s = \\ez => det.s ++ cn.s ! aEzafa ! det.n ++ xs.s ;
    a = agrP3 det.n
    } ;
  CNNumNP cn i = cn ** {
    s = \\ez => cn.s ! aEzafa ! Sg ++ i.s ;
    a = agrP3 Sg
    } ;

  SymbS sy = sy ;
  SymbNum sy = { s = sy.s ; n = Pl } ;
  SymbOrd sy = { s = sy.s ++ "wN" ; n = Pl; isNum=False} ;

lincat

  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;

  BaseSymb = infixSS "تE" ;
  ConsSymb = infixSS "" ;

--oper
    -- Note: this results in a space before 's, but there's
    -- not mauch we can do about that.
--    addGenitiveS : Str ;
--    addGenitiveS s =
--     s ++ "از" ;


}
