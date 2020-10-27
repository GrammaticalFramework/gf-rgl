--# -path=.:../abstract:../common:../prelude: -- partially extracted from DictVerbsGerAbs
abstract TestLexiconGerAbs = Lexicon ** {

fun
  aendern_rV : V ;
  anstrengen_rV : V ;

  gedenken_gen_V2 : V2 ;
  bedienen_gen_rV2 : V2 ;
  stuetzen_auf_rV2 : V2 ;
  ergeben_dat_rV2 : V2 ;
  merken_rV2 : V2 ;

  anklagen_gen_V3 : V3 ;
  erklaeren_dat_V3 : V3 ;
  lehren_V3 : V3 ;
  erinnern_an_V3 : V3 ;
  danken_dat_fuer_V3 : V3 ;
  debattieren_mit_ueber_V3 : V3 ;

  abschauen_bei_rV3 : V3 ;
  leihen_von_rV3 : V3 ;

  entschuldigen_bei_fuer_rV3 : V3 ;
  raechen_am_fuer_rV3 : V3 ;

  neugierig_auf_A2 : A2 ;

  wagen_VV : VV ;
  versuchen_VV : VV ;

  helfen_V2V : V2V ;          -- -aux(zu-inf), object control
  warnen_V2V : V2V ;          -- -aux,         object control
  versprechen_dat_V2V : V2V ; -- -aux,         subject control
  lassen_V2V : V2V ;          -- +aux(inf),    object control

cat 
  V4 ;
fun
  kaufen_bei_fuer_V4 : V4 ;
  mieten_von_fuer_V4 : V4 ;

}
