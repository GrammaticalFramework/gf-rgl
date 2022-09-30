--# -path=.:../slovak:../common:../abstract:../prelude

resource TryHrv = SyntaxHrv, LexiconHrv, ParadigmsHrv -[mkAdv, mkAdN, mkOrd, mkDet, mkQuant]** 
  open (P = ParadigmsHrv) in {

-- oper

--  mkAdv = overload SyntaxHrv {
--    mkAdv : Str -> Adv = P.mkAdv ;
--  } ;

}

