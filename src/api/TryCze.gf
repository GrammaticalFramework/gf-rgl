--# -path=.:../czech:../common:../abstract:../prelude

resource TryCze = ExtraCze, SyntaxCze-[mkVoc], LexiconCze, ParadigmsCze -[mkAdv,mkAdN,mkIAdv,mkDet,mkIDet,mkIP,mkQuant,mkCard,mkPConj,mkVoc]**
  open (P = ParadigmsCze),(S = SyntaxCze) in {

oper
  mkAdv = overload SyntaxCze {
    mkAdv : Str -> Adv = P.mkAdv ;
  } ;
  mkVoc = overload {
    mkVoc : NP -> Voc = S.mkVoc ;
    mkVoc : Str -> Voc = P.mkVoc ;
  } ;
  mkIP = overload SyntaxCze {
    mkIP : Str -> IP = P.mkIP ;
  } ;

}
