--# -path=.:../abstract:../common:../prelude:               HL 19/06/2019
-- Some english interpretations of the verbs in TestLexiconGerAbs to test
-- translations and corresponding c2,c3,c4-objects under Slash?V3, Slash?V4.

concrete TestLexiconEng of TestLexiconGerAbs = 
  LexiconEng ** open (R=ResEng), (P=Prelude), ParadigmsEng, (I=IrregEng)
in {

lincat 
  V4 = R.Verb ** {c2,c3,c4 : Str} ;

oper
  reflV2 : V -> Prep -> V2 ;         -- reflexive, with prep-object
  reflV2 v p = prepV2 (reflV v) p ;

  reflV3 : V -> Prep -> Prep -> V3 ; -- reflexive, with two prep-objects
  reflV3 v p q = mkV3 (reflV v) p q ;

  -- quaternary verbs:
  mkV4 : V -> Prep -> Prep -> Prep -> V4 = 
         \v,p2,p3,p4 -> lin V4 (v ** { c2=p2.s ; c3=p3.s ; c4=p4.s }) ;
  dirV4 : V -> Prep -> Prep -> V4 = \v,c,d -> mkV4 v noPrep c d ;
  -- control verbs:
  defaultV2V : V -> V2V = \v -> lin V2V (dirV2 v ** {c3=[] ; typ = R.VVInf}) ;

lin
  aendern_rV = (regV "change") ;
  anstrengen_rV = let v : R.Verb = (irregV "make" "made" "made") ; 
                      compl : Str = "an effort" 
    in {s = \\vf => v.s!vf ++ compl ; isRefl = P.False ; p = []} ;

  gedenken_gen_V2  = dirV2 (regV "remember") ;
  bedienen_gen_rV2 = dirV2 (regV "use") ;
  stuetzen_auf_rV2 = mkV2 (irregV "rely" "relied" "relied") (mkPrep "on") ;
  ergeben_dat_rV2 = mkV2 (regV "surrender") (mkPrep "to") ;
  merken_rV2 = dirV2 (regV "remember") ;

  anklagen_gen_V3 = dirV3 (regV "accuse") (mkPrep "of") ;
  erklaeren_dat_V3 = dirV3 (regV "explain") (mkPrep "to") ;
  erinnern_an_V3 = dirV3 (regV "remind") (mkPrep "of") ;
  danken_dat_fuer_V3 = dirV3 (regV "thank") (mkPrep "for") ;
  debattieren_mit_ueber_V3 = mkV3 (regV "debate") (mkPrep "with") (mkPrep "about") ;
  lehren_V3 = mkV3 (irregV "teach" "taught" "taught") noPrep noPrep ;

  abschauen_bei_rV3 = dirV3 (regV "copy") (mkPrep "from") ; 
  leihen_von_rV3 = dirV3 (regV "borrow") (mkPrep "from") ; 

  entschuldigen_bei_fuer_rV3 = mkV3 (regV "apologize") (mkPrep "to") (mkPrep "for") ; 
  raechen_am_fuer_rV3 = mkV3 (regV "revenge") (mkPrep "on") (mkPrep "for") ; 

  kaufen_bei_fuer_V4 = dirV4 (irregV "buy" "bought" "bought") (mkPrep "from") (mkPrep "for") ;
  mieten_von_fuer_V4 = dirV4 (regV "rent") (mkPrep "from") (mkPrep "for") ;

  neugierig_auf_A2 = mkA2 (regA "curious") (mkPrep "about") ;

  wagen_VV = mkVV (regV "dare") ;                       -- typ=VVInf
  versuchen_VV = mkVV (irregV "try" "tried" "tried") ;  -- typ=VVInf
  helfen_V2V = defaultV2V (regV "help") ;
  warnen_V2V = defaultV2V (regV "warn") ;               -- typ=VVInf
  versprechen_dat_V2V = defaultV2V (regV "promise") ;   -- typ=VVInf
  lassen_V2V = ParadigmsEng.mkV2V (I.let_V) ;           -- typ=VVAux

}
