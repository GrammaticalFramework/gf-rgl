--# -path=.:../czech:../common:../abstract:../prelude

resource TryCze = SyntaxCze, LexiconCze, ParadigmsCze -[mkAdv, mkDet,mkQuant]** 
  open (P = ParadigmsCze) in {

-- oper

--  mkAdv = overload SyntaxCze {
--    mkAdv : Str -> Adv = P.mkAdv ;
--  } ;

}

