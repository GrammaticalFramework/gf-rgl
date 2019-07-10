--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangGer of TestLang = 
  GrammarGer,
  LexiconGer
  , TestLexiconGer
  , ConstructionGer
  ** open (R=ResGer),Prelude,(P=ParadigmsGer) in {

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

    PassVPSlash vp = 
      let c = case <vp.c2.c,vp.c2.isPrep> of {
            <R.NPC R.Acc,False> => R.NPC R.Nom ;
            _ => vp.c2.c}
      in R.insertObj (\\_ => (PastPartAP vp).s ! R.APred) (R.predV R.werdenPass) **
      {subjc = vp.c2 ** {c= c}} ;
                -- regulates passivised object: accusative objects -> nom; all others: same case
		-- this also gives "mit dir wird gerechnet" ;
		-- the alternative linearisation ("es wird mit dir gerechnet") is not implemented
       -- HL: does not work for vp = (Slash2V3 v np): uns wird den Beweis erklärt
    PastPartAP vp = {
      s = \\af => (vp.nn ! R.agrP3 R.Sg).p1 ++ (vp.nn ! R.agrP3 R.Sg).p2 ++ 
        (vp.nn ! R.agrP3 R.Sg).p3 ++ (vp.nn ! R.agrP3 R.Sg).p4 ++ vp.adj ++ vp.a2 
        ++ vp.inf ++ vp.ext ++ vp.infExt ++ vp.s.s ! R.VPastPart af ;
      isPre = True ;
      c = <[],[]> ;
      adj = [] ;
      ext = [] 
      } ;

    Pass2V3 v np = -- HL 7/19: making the (active) direct object to the (passive) subject
      let vps : VPSlash = 
            R.insertObj (\\_ => (v.s ! R.VPastPart R.APred)) (R.predV R.werdenPass)
            ** { subjc = R.PrepNom ; c2 = v.c3 } 
      in R.insertObjNP np vps.c2 vps ;

    Pass3V3 v np = -- HL 7/19: making the (active) indirect object to the (passive) subject
      let bekommenPass : R.Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") ;
          vps : VPSlash = 
            R.insertObj (\\_ => (v.s ! R.VPastPart R.APred)) (R.predV bekommenPass)
            ** { subjc = R.PrepNom ; c2 = v.c2 } 
      in R.insertObjNP np vps.c2 vps ;
      

} ;
