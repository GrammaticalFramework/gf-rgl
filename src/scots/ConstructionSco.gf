concrete ConstructionSco of Construction = ConstructionEng - [has_age_VP] **
  open SyntaxSco, SymbolicSco, ParadigmsSco in {

lin has_age_VP card =
  mkVP (mkAP
    (lin AdA (mkUtt (mkNP <lin Card card : Card> (mkN "year" "year"))))
    (mkA "auld")) ;

}
