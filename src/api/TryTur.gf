--# -path=.:../turkish:../common:../abstract:../prelude

resource TryTur = SyntaxTur, LexiconTur, ParadigmsTur - [mkAdN,mkAdv,mkNum,mkQuant] **
  open (P = ParadigmsTur), (R = ResTur) in {

oper
  mkAdv = overload SyntaxTur {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;

}
