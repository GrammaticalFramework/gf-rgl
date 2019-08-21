--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangGer of TestLang = 
  GrammarGer - [PassV2] -- to improve these ,ComplVV,SlashVV,SlashV2V
  , TestLexiconGer
--  , ConstructionGer
  ** open ResGer,Prelude,(P=ParadigmsGer) in {

flags startcat = Phr ; unlexer = text ; lexer = text ;
  lincat
    VPSlashSlash = CatGer.VPSlash ** {c3 : Preposition} ;
  lin 
    ReflVPSlash v3 = (insertObjRefl (predVc v3) ** {c2 = v3.c3 ; ctrl = SubjC}); -- reflexive use of v:V3, untested

    -- SlashV3a v = (predVc v) ** {c3 = v.c3} ;
{- save time:
    Slash2V4 v np = (lin VPSlash (insertObjNP np v.c2 (predV v) ** {c2 = v.c3 ; missingAdv = True})) ** { c3 = v.c4 } ; 
    Slash3V4 v np = (lin VPSlash (insertObjNP np v.c3 (predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c4 } ;
    Slash4V4 v np = (lin VPSlash (insertObjNP np v.c4 (predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c3 } ;

--    ComplSlashSlash vpss np = insertObjNP np vpss.c2 vpss ** {c2 = vpss.c3 ; missingAdv = True } ;
time saved -}
    -- linref
    --   V4 = \v -> useInfVP False (predV v) ++ v.c2.s ++ v.c3.s ++ v.c4.s ;
{- save time
    PassV2 v = -- insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass) ;
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} -- acc object -> nom; all others: same PCase
      in insertObjc (\\_ => v.s ! VPastPart APred) (predV werdenPass) ** { subjc = v.c2 ** {c = c} } ;

    PassV2Q v q = 
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp = insertObjc (\\_ => v.s ! VPastPart APred) (predV werdenPass) 
            ** { subjc = v.c2 ** {c = c} } 
      in insertExtrapos (bindComma ++ q.s ! QIndir) vp ;

    PassV2S v s = 
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp = insertObjc (\\_ => v.s ! VPastPart APred) (predV werdenPass) 
            ** { subjc = v.c2 ** {c = c} } 
      in insertExtrapos (bindComma ++ conjThat ++ s.s ! Sub) vp ;

    PassV2V v vp = 
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp2 = insertObjc (\\_ => v.s ! VPastPart APred) (predV werdenPass) 
            ** { subjc = v.c2 ** {c = c} } 
      in insertExtrapos (bindComma ++ (useInfVP False vp)) vp2 ; -- misses subject agr for vp = ReflVP vps

    PassVPSlash vp = 
      let c = case <vp.c2.c,vp.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => vp.c2.c}
      in insertObjc (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass) 
      ** {ext = vp.ext ; subjc = vp.c2 ** {c = c}} ;
                -- regulates passivised object: accusative objects -> nom; all others: same case
		-- this also gives "mit dir wird gerechnet" ;
		-- the alternative linearisation ("es wird mit dir gerechnet") is not implemented
       -- HL: does not work for vp = (Slash2V3 v np): uns wird den Beweis erklärt
       --                       vp = (SlashV2V v2v reflVP): wir werden gebeten, uns zu fragen , ob S
    PastPartAP vp = {
      s = \\af => (vp.nn ! agrP3 Sg).p1 ++ (vp.nn ! agrP3 Sg).p2 ++ 
        (vp.nn ! agrP3 Sg).p3 ++ (vp.nn ! agrP3 Sg).p4 ++ vp.adj ++ vp.a2 
        ++ vp.inf.s ++ vp.infExt ++ vp.s.s ! VPastPart af ;
      isPre = True ;
      c = <[],[]> ;
      adj = [] ;
      ext = vp.ext 
      } ;

    Pass2V3 v np = -- HL 7/19: making the (active) direct object to the (passive) subject
      let vps = insertObj (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { subjc = PrepNom ; c2 = v.c3 }
      in insertObjNP np vps.c2 vps ;

    Pass3V3 v np = -- HL 7/19: making the (active) indirect object to the (passive) subject
      let bekommen : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") ;
          vps = insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommen)
            ** { subjc = PrepNom ; c2 = v.c2 } 
      in insertObjNP np vps.c2 vps ;
      
    Pass2V4 v np = 
      let vps = -- : VPSlashSlash = 
            insertObj (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { subjc = PrepNom ; c2 = v.c3 ; c3 = v.c4 } 
      in (insertObjNP np vps.c3 vps) ;

    -- Todo: Pass?V2S, Pass?V2Q, PassVS, PassVQ Pass?V2V
-}

    SlashV2Vneg v vp =   -- warnen, (\agr => sich!agr das Buch nicht zu merken) 
      let 
        vps = (predVGen v.isAux v) ** { c2 = v.c2 ; ctrl = v.ctrl } ;
        vpi = infzuVP v.isAux v.ctrl Simul Neg vp ;
        comma = case orB vp.isAux (case vp.inf.ctrl of { NoC => True ; _ => False }) of {True => [] ; _ => bindComma} ;
        embeddedInf : Agr => Str = case vp.inf.isAux of {
          True  => \\agr => comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;  -- ihn es lesen (zu) lassen
          False => \\agr => comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 }  -- ihn (zu) bitten , es zu lesen
      in 
      insertExtrapos vpi.ext (
        insertInf vpi.pred (   
          insertInfExtraObj vpi.objs (
            insertInfExtraInf embeddedInf vps))) ;

}
