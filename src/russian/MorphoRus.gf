resource MorphoRus = ResRus ** open Prelude in {
flags coding=utf8 ;
oper
  to_exist = guessVerbForms Imperfective Intransitive "существовать" "существую" "существует";
  everybody = pronFormsPronoun vse ;
  everything = pronFormsPronoun vse_ina ;
  what_sg = doChPron "ч" (Ag (GSg Neut) P3) Inanimate ;
  what_pl = doChPron "ч" (Ag GPl P3) Inanimate ;
  something = pronFormsPronoun ((appendToIP what_sg (BIND ++ "-то")) ** {nPrefix=False}) ;
  nothing = pronFormsPronoun ((doChPron "нич" (Ag (GSg Neut) P3) Inanimate) ** {nPrefix=False}) ;
  who_sg = doKPron "к" (Ag (GSg Masc) P3) Animate ;
  who_pl = doKPron "к" (Ag GPl P3) Animate ;
  somebody = pronFormsPronoun ((appendToIP who_sg (BIND ++ "-то")) ** {nPrefix=False}) ;
  nobody = pronFormsPronoun ((doKPron "ник" (Ag (GSg Masc) P3) Animate) ** {nPrefix=False}) ;
  such = adjFormsAdjective (makeAdjectiveForms "такой" "" "3b" PrefFull) ;
}
