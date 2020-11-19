--# -path=.:../catalan:../romance:../common:../abstract:../prelude

resource TryCat = SyntaxCat, LexiconCat, ParadigmsCat - [mkAdv] ** 
  open (P = ParadigmsCat) in {

oper

  mkAdv = overload SyntaxCat {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;


}
