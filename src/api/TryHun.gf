--# -path=.:../hungarian:../common:../abstract:../prelude

resource TryHun = SyntaxHun-[mkAdN], LexiconHun, ParadigmsHun - [mkAdv,mkAdN] **
  open (P = ParadigmsHun) in {

oper
  mkAdv = overload SyntaxHun {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

  mkAdN = overload {
    mkAdN : CAdv -> AdN = SyntaxHun.mkAdN ;
    mkAdN : Str -> AdN = P.mkAdN ;
  } ;

}

