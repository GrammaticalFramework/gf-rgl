--# -path=.:../greek:../common:../abstract:../prelude

resource TryGre = SyntaxGre-[mkAdN,mkVoc], LexiconGre, ParadigmsGre - [mkAdN,mkAdv,mkVoc] **
  open (P = ParadigmsGre) in {

oper
  mkAdv = overload SyntaxGre {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkVoc = overload {
    mkVoc : NP -> Voc = SyntaxGre.mkVoc ;
    mkVoc : Str -> Voc = P.mkVoc ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxGre.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}
