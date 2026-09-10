--# -path=.:../maltese:../common:../abstract:../prelude

resource TryMlt = SyntaxMlt, LexiconMlt, ParadigmsMlt - [mkAdN, mkAdv,mkCard,mkDet,mkIAdv,mkIDet,mkOrd,mkPConj,mkQuant,mkVoc] **
  open (P = ParadigmsMlt) in {

}
