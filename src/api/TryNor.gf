--# -path=.:../norwegian:../scandinavian:../common:../abstract:../prelude

resource TryNor = SyntaxNor-[mkAdN], LexiconNor, ParadigmsNor - [mkAdv,mkAdN] **
  open (P = ParadigmsNor) in {

oper
  mkAdv = overload SyntaxNor {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxNor.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}
