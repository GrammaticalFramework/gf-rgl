--# -path=.:../swedish:../scandinavian:../common:../abstract:../prelude

resource TrySwe = SyntaxSwe, LexiconSwe, ParadigmsSwe - [mkAdv] ** 
  open (P = ParadigmsSwe) in {

oper

  mkAdv = overload SyntaxSwe {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

}
