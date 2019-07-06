concrete VerbGer of Verb = CatGer ** open Prelude, ResGer, Coordination in {

  flags optimize=all_subs ;

  lin
    UseV = predV ;

    ComplVV v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ;
      in
       insertExtrapos vpi.p4 (
        insertInfExt vpi.p3 (
          insertInf vpi.p2 (
            insertObjc vpi.p1 vps))) ;

    ComplVS v s = 
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (predV v) ;
    ComplVQ v q = 
      insertExtrapos (comma ++ q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertAdj (v.c2.s ++ ap.s ! APred) ap.c ap.ext (predV v) ; -- changed

    SlashV2a v = (predVc v) ** {missingAdv = True} ; -- HL 12/6/2019 for reflexive verbs with objects, rV2:V2, rV3:V3
      
    Slash2V3 v np = insertObjNP np v.c2 (predV v) ** {c2 = v.c3 ; missingAdv = True} ; 
    Slash3V3 v np = insertObjNP np v.c3 (predV v) ** {c2 = v.c2 ; missingAdv = True} ;

    SlashV2S v s = 
      insertExtrapos (conjThat ++ s.s ! Sub) (predVc v) ;
    SlashV2Q v q = 
      insertExtrapos (q.s ! QIndir) (predVc v) ;
    SlashV2V v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = v.c2} ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 vps)) ;

    SlashV2A v ap = 
      insertAdj (ap.s ! APred) ap.c ap.ext (predVc v) ;

    ComplSlash vps np =
      let vp = insertObjNP np vps.c2 vps ;
       in case vp.missingAdv of {
        True  => vp ;
        False => objAgr np vp } ; -- IL 24/04/2018 force reflexive in the VPSlash to take the agreement of the object introduced by ComplSlash.

    SlashVV v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2} ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 vps)) ;

    SlashV2VNP v np vp =
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = v.c2} ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 (
            insertObj (\\_ => appPrepNP v.c2 np) vps))) ;
--            insertObjNP v.c2 np vps))) ;

    UseComp comp =
       insertExtrapos comp.ext (insertObj comp.s (predV sein_V)) ; -- agr not used
    -- adj slot not used here for e.g. "ich bin alt" but same behaviour as NPs?
	-- "ich bin nicht alt" "ich bin nicht Doris" 

    UseCopula = predV sein_V ;

    CompAP ap = {s = \\_ => ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ; ext = ap.ext} ;
    CompNP np = {s = \\_ => np.s ! NPC Nom ++ np.adv ++ np.rc ; ext = np.ext} ;
    CompAdv a = {s = \\_ => a.s ; ext = []} ;

    CompCN cn = {s = \\a => case numberAgr a of {
        Sg => "ein" + pronEnding ! GSg cn.g ! Nom ++ cn.s ! Strong ! Sg ! Nom ;
        Pl => cn.s ! Strong ! Pl ! Nom
        } ;
		ext = []
      } ;

    AdvVP vp adv = insertAdv adv.s vp ;
    ExtAdvVP vp adv = insertAdv (embedInCommas adv.s) vp ;

    AdVVP adv vp = insertAdv adv.s vp ; -- not AdV 27/5/2012: nicht immer

    AdvVPSlash vp adv = vp ** insertAdv adv.s vp ;
    AdVVPSlash adv vp = vp ** insertAdv adv.s vp ;

    -- ReflVP vp = insertObj (\\a => appPrep vp.c2 
    --   (\\k => usePrepC k (\c -> reflPron ! a ! c))) vp ;
    ReflVP vp = insertObjRefl vp ; -- HL, 19/06/2019

    PassV2 v = insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass) ;

{- HL: The construction VPSlashPrep : VP -> Prep -> VPSlash does not exist
   in German. In abstract/Verb.gf, the example

    VPSlashPrep : VP -> Prep -> VPSlash ;  -- live in (it)

   (with live_V:V) indicates that, here, you consider Prep=AdvSlash,
   so to speak, for building a compact version of relative clauses:

        the city we live in : NP

   from  "we live (in the city : Adv) : Cl"

   But in German we cannot move the NP part of an Adv, we only have the
   full relative clauses like

        die Stadt, in der wir leben,
        die Stadt, worin wir leben,    --contracted Prep+Rel

   but nothing like "die Stadt wir leben in".
-}
    VPSlashPrep vp prep = vp ** {c2 = prep ; missingAdv = True} ;

}
