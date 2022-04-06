--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/english
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangEng of TestLang = 
  GrammarEng
  , TestLexiconEng
  , ConstructionEng
  ** open (R=ResEng), (P=ParadigmsEng), Prelude, (E=ExtendEng)
  in {

  flags 
    startcat = Phr ; unlexer = text ; lexer = text ;

  lin
    SlashV2Vneg v vp = 
      R.insertObjc (\\a => v.c3 ++ R.infVP v.typ vp False R.Simul (R.CNeg True) a) (R.predVc v) ;

  lincat
    VPSlashSlash = VPSlash ** {c3 : Str} ;
  lin 
    ReflVPSlash v3 = (R.predVc ((P.reflV (lin V v3)) ** {c2 = v3.c3 ; missingAdv = True})); 

    ComplSlashSlash vpss np = R.insertObjc 
      (appPrep vpss.c2 (lin NP np)) (vpss ** {c2 = vpss.c3 ; missingAdv = True }) ;

    Slash2V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c2 (lin NP np)) (R.predVc v) 
                                    ** {c2 = v.c3 ; missingAdv = True})) ** { c3 = v.c4 } ; 
    Slash3V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c3 (lin NP np)) (R.predVc v) 
                                    ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c4 } ;
    Slash4V4 v np = (lin VPSlash (R.insertObjc (appPrep v.c4 (lin NP np)) (R.predVc v) 
                                    ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c3 } ;

  oper
    appPrep : Str -> NP -> (R.Agr => Str) = \p,np -> \\_ => p ++ np.s ! R.NPAcc ;

    -- Passive
  lin
    Pass2V3 v np = 
      let vps = R.insertObj (\\_ => v.s ! R.VPPart ++ v.p) (R.predAux R.auxBe) ** {c2 = v.c3} 
      in R.insertObj (\\_ => vps.c2 ++ np.s ! R.NPAcc) vps ;

    Pass3V3 v np = 
      let vps = R.insertObj (\\_ => v.s ! R.VPPart ++ v.p) (R.predAux R.auxBe) ** {c2 = v.c2} 
      in R.insertObj (\\_ => vps.c2 ++ np.s ! R.NPAcc) vps ;

    PastPartAP = E.PastPartAP ;
    PassVPSlash = E.PassVPSlash ;

    Pass2V4 v np =
      let vpss = R.insertObj (\\_ => v.s ! R.VPPart ++ v.p) (R.predAux R.auxBe) ** {c2 = v.c3 ; c3 = v.c4} 
      in R.insertObj (\\_ => vpss.c3 ++ np.s ! R.NPAcc) vpss ** {c2 = vpss.c2 ; 
                                                                 missingAdv = True ; 
                                                                 gapInMiddle = False } ;

} ;
