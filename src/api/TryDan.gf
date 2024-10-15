--# -path=.:../danish:../scandinavian:../common:../abstract:../prelude

resource TryDan = SyntaxDan-[mkAdN], LexiconDan, ParadigmsDan - [mkAdv,mkAdN] **
  open (P = ParadigmsDan) in {

oper
  mkAdv = overload SyntaxDan {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxDan.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}
