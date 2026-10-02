--# -path=.:../czech:../common:../abstract:../prelude

resource TryCze = ExtraCze, SyntaxCze, LexiconCze, ParadigmsCze -[mkAdv,mkAdN,mkIAdv,mkDet,mkIDet,mkIP,mkQuant,mkCard,mkPConj,mkVoc]**
  open (P = ParadigmsCze) in {

-- oper

--  mkAdv = overload SyntaxCze {
--    mkAdv : Str -> Adv = P.mkAdv ;
--  } ;

}
