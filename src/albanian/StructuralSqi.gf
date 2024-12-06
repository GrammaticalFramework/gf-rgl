concrete StructuralSqi of Structural = CatSqi ** open ResSqi, ParadigmsSqi in {

lin i_Pron = mkPron "unë" "mua" "mua" "meje" "më" "më" (GSg Masc) P1 ;
lin youSg_Pron = mkPron "ti" "ty" "ty" "teje" "të" "të" (GSg Masc) P2 ;
lin he_Pron = mkPron "ai" "atë" "atij" "atij" "e" "i" (GSg Masc) P3 ;
lin she_Pron = mkPron "ajo" "atë" "asaj" "asaj" "e" "i" (GSg Fem) P3 ;
lin it_Pron = mkPron "ai" "atë" "atij" "atij" "e" "i" (GSg Masc) P3 ;
lin we_Pron = mkPron "ne" "ne" "neve" "nesh" "na" "na" GPl P1 ;
lin youPl_Pron = mkPron "ju" "ju" "juve" "jush" "ju" "ju" GPl P2 ;
lin they_Pron = mkPron "ata" "ata" "atyre" "atyre" "i" "u" GPl P3 ;
lin this_Quant = mkQuant "ky"    "këta"     "kjo"   "këto"
                         "këtë"  "këtyre"   "këtë"  "këtyre"
                         "këtij" "këtyre"   "kësaj" "këtyre"
                         "këtij" "këtyre"   "kësaj" "këtyre" ;
lin that_Quant = mkQuant "ai"   "ata"   "ajo"  "ato"
                         "atë"  "ata"   "atë"  "ato"
                         "atij" "atyre" "asaj" "atyre"
                         "atij" "atyre" "asaj" "atyre" ;
}
