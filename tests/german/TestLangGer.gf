--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangGer of TestLang = 
  GrammarGer,
  LexiconGer
  , TestLexiconGer
  , ConstructionGer
  ** open (R=ResGer),Prelude in {

flags startcat = Phr ; unlexer = text ; lexer = text ;
  lincat
    VPSlashSlash = VPSlash ** {c3 : R.Preposition} ;
  lin 
    ReflVPSlash v3 = (R.insertObjRefl (R.predVc v3) ** {c2 = v3.c3 ; missingAdv = True}); -- reflexive use of v:V3, untested

    -- SlashV3a v = (R.predVc v) ** {c3 = v.c3} ;

    Slash2V4 v np = (lin VPSlash (R.insertObjNP np v.c2 (R.predV v) ** {c2 = v.c3 ; missingAdv = True})) ** { c3 = v.c4 } ; 
    Slash3V4 v np = (lin VPSlash (R.insertObjNP np v.c3 (R.predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c4 } ;
    Slash4V4 v np = (lin VPSlash (R.insertObjNP np v.c4 (R.predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c3 } ;

    ComplSlashSlash vpss np = R.insertObjNP np vpss.c2 vpss ** {c2 = vpss.c3 ; missingAdv = True } ;

    -- linref
    --   V4 = \v -> useInfVP False (R.predV v) ++ v.c2.s ++ v.c3.s ++ v.c4.s ;

} ;
