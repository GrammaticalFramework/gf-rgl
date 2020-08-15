--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/english
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangEng of TestLang = 
  GrammarEng,
  LexiconEng
  , TestLexiconEng
  , ConstructionEng
  ** open (R=ResEng),(P=ParadigmsEng),Prelude in {

  flags 
    startcat = Phr ; unlexer = text ; lexer = text ;

  lincat
    VPSlashSlash = VPSlash ** {c3 : Str} ;
  lin 
    ReflVPSlash v3 = (R.predVc ((P.reflV (lin V v3)) ** {c2 = v3.c3 ; missingAdv = True})); 

    ComplSlashSlash vpss np = R.insertObjc (appPrep vpss.c2 (lin NP np)) (vpss ** {c2 = vpss.c3 ; missingAdv = True }) ;

    Slash2V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c2 (lin NP np)) (R.predVc v) ** {c2 = v.c3 ; missingAdv = True})) ** { c3 = v.c4 } ; 
    Slash3V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c3 (lin NP np)) (R.predVc v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c4 } ;
    Slash4V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c4 (lin NP np)) (R.predVc v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c3 } ;

  oper
    appPrep : Str -> NP -> (R.Agr => Str) = \p,np -> \\_ => p ++ np.s ! R.NPAcc ;
} ;
