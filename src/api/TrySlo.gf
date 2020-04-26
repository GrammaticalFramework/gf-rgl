--# -path=.:../slovak:../common:../abstract:../prelude

resource TrySlo = SyntaxSlo, LexiconSlo, ParadigmsSlo -[mkAdv, mkDet,mkQuant]** 
  open (P = ParadigmsSlo) in {

-- oper

--  mkAdv = overload SyntaxSlo {
--    mkAdv : Str -> Adv = P.mkAdv ;
--  } ;

}

