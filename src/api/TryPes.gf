--# -path=.:../persian:../common:../abstract:../prelude

resource TryPes = SyntaxPes-[mkAdN], LexiconPes, ParadigmsPes -[mkDet,mkQuant,mkAdv,mkAdN,mkOrd,mkQuant,mkVoc]**
  open (P = ParadigmsPes) in {

oper
  mkAdv = overload SyntaxPes {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxPes.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}
