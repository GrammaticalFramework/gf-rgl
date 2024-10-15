--# -path=.:../nynorsk:../scandinavian:../common:../abstract:../prelude

resource TryNno = SyntaxNno-[mkAdN], LexiconNno, ParadigmsNno - [mkAdv,mkAdN] **
  open (P = ParadigmsNno) in {

oper
  mkAdv = overload SyntaxNno {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxNno.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}
