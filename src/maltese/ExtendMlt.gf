concrete ExtendMlt of Extend =
  CatMlt ** ExtendFunctor - [
                 ComplDirectVS,
                 ComplDirectVQ,
                 iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron,
                 theyFem_Pron, youPolFem_Pron
              ]
              with (Grammar = GrammarMlt) ** open ParadigmsMlt, MorphoMlt, ResMlt in {

lin iFem_Pron = mkPron "jien"  "i" singular P1 feminine ; --- also JIENA
    youFem_Pron = mkPron "int" "ek" singular P2 feminine ; --- also INTI
    weFem_Pron = mkPron "aħna"  "na"  plural P1 feminine ;
    youPlFem_Pron = mkPron "intom" "kom" plural P2 feminine ;
    theyFem_Pron = mkPron "huma" "hom" plural P3 feminine ;
    youPolFem_Pron = mkPron "int" "ek" singular P2 feminine ; --- also INTI

}
