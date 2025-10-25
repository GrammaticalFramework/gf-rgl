--# -path=.:../basque:../common:../abstract:../prelude

resource TryEus = SyntaxEus-[mkVoc], LexiconEus, ParadigmsEus - [mkAdv,mkAdN,mkDet,mkQuant,mkPConj,mkVoc] **
  open (P = ParadigmsEus) in {

oper
  mkAdv = overload SyntaxEus {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkVoc = overload {
    mkVoc : NP -> Voc = SyntaxEus.mkVoc ;
    mkVoc : Str -> Voc = P.mkVoc ;
  } ;

}
