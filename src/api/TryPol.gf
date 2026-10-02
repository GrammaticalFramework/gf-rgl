--# -path=.:../polish:../common:../abstract:../prelude

resource TryPol = SyntaxPol, LexiconPol, ParadigmsPol - [mkAdv,mkAdN,mkIAdv,mkCard,mkDet,mkIDet,mkQuant,mkPConj] **
  open (P = ParadigmsPol) in {

--oper

--  mkAdv = overload SyntaxPol {
--    mkAdv : Str -> Adv = P.mkAdv ;
--  } ;

}
