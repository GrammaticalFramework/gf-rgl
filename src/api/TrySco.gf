--# -path=.:../english:../common:../abstract:../prelude

resource TrySco = SyntaxSco-[mkAdN], LexiconSco, ParadigmsSco - [mkAdv,mkAdN,mkOrd,mkQuant,mkVoc] **
  open (P = ParadigmsEng) in {

oper

  mkAdv = overload SyntaxSco {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxSco.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

  mkOrd = overload SyntaxSco {
    mkOrd : Str -> Ord = P.mkOrd ;
  } ;


}
