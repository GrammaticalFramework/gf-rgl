--# -path=.:../icelandic:../common:../abstract:../prelude

resource TryIce = SyntaxIce-[mkVoc], LexiconIce, ParadigmsIce - [mkAdv,mkAdN,mkOrd,mkCard,mkQuant,mkDet,mkIDet,mkVoc] **
  open (P = ParadigmsIce) in {

oper
  mkVoc = overload {
    mkVoc : NP -> Voc = SyntaxIce.mkVoc ;
    mkVoc : Str -> Voc = P.mkVoc ;
  } ;

}
