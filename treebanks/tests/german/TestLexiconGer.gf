--# -path=.:../abstract:../common:../prelude: 

concrete TestLexiconGer of TestLexiconGerAbs = 
  CatGer ** open (R=ResGer), (P=Prelude), ParadigmsGer 
in {

lincat 
  V4 = R.Verb ** {c2,c3,c4 : R.Preposition} ;

oper
  reflV2 : V -> Case -> Prep -> V2 ; -- reflexive, with case and prep-object
  reflV2 v c p = prepV2 (reflV v c) p ;

  reflV3 : V -> Case -> Prep -> Prep -> V3 ; -- reflexive, with case, and prep-objects
  reflV3 v c p q = mkV3 (reflV v c) p q ;

  bei_Prep   = mkPrep "bei" dative ; 
  fuer_Prep  = mkPrep "für" accusative ;

  -- quaternary verbs:
  mkV4 : V -> Prep -> Prep -> Prep -> V4 = 
         \v,p2,p3,p4 -> lin V4 (v ** { c2=p2 ; c3=p3 ; c4=p4 }) ;
  dirV4 : V -> Prep -> Prep -> V4 = \v,c,d -> mkV4 v accPrep c d ;

lin
  aendern_rV = reflV (regV "ändern") accusative ;
  anstrengen_rV = reflV (prefixV "an" (regV "strengen")) accusative ;

  bedienen_gen_rV2 = reflV2 (regV "bedienen") accusative genPrep ;
  stuetzen_auf_rV2 = reflV2 (regV "stützen") accusative (mkPrep "auf" accusative) ;
  ergeben_dat_rV2 = reflV2 (irregV "ergeben" "ergibt" "ergab" "ergäbe" "ergeben") accusative datPrep ;
  merken_rV2 = reflV2 (regV "merken") dative accPrep ;

  erklaeren_dat_V3 = accdatV3 (irregV "erklären" "erklärt" "erklärte" "erklärte" "erklärt") ;
  anklagen_gen_V3 = dirV3 (prefixV "an" (regV "klagen")) genPrep ;
  erinnern_an_V3 = dirV3 (irregV "erinnern" "erinnert" "erinnerte" "erinnerte" "erinnert") (mkPrep "an" accusative) ; 
  danken_dat_fuer_V3 = mkV3 (regV "danken") datPrep (mkPrep "für" accusative) ;
  lehren_V3 = dirV3 (regV "lehren") accPrep ;

  abschauen_bei_rV3 = reflV3 (prefixV "ab" (irregV "schauen" "schaut" "schaute" "schaute" "geschaut")) dative accPrep bei_Prep ;
  leihen_von_rV3 = reflV3 (irregV "leihen" "leiht" "lieh" "liehe" "geliehen") dative accPrep von_Prep ;

  entschuldigen_bei_fuer_rV3 = 
    reflV3 (irregV "entschuldigen" "entschuldigt" "entschuldigte" "entschuldigte" "entschuldigt") accusative bei_Prep fuer_Prep ;
  raechen_am_fuer_rV3 = reflV3 (regV "rächen") accusative (mkPrep "an" dative) fuer_Prep ;

  kaufen_bei_fuer_V4 = dirV4 (regV "kaufen") bei_Prep fuer_Prep ;
  mieten_von_fuer_V4 = dirV4 (regV "mieten") von_Prep fuer_Prep ;

  neugierig_auf_A2 = mkA2 (mk3A "neugierig" "neugieriger" "neugierigste") (mkPrep "auf" accusative) ;

}
