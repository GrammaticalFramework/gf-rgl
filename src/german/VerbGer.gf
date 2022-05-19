concrete VerbGer of Verb = CatGer ** open Prelude, ResGer, Coordination in {

  flags optimize=all_subs ;

  lin
    UseV = predV ;
{-
    ComplVV v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ;
      in
       insertExtrapos vpi.p4 (
        insertInfExt vpi.p3 (
          insertInf vpi.p2 (
            insertObjc vpi.p1 vps))) ;
-}  

    ComplVV v vp = -- HL 3/22: leave inf-complement in-place, extract infzu-complement
      let
        vps = predVGen v.isAux v ; -- e.g. will.isAux=True | wagt.isAux=False
        inf = mkInf v.isAux Simul Pos vp
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ;

    ComplVS v s = 
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (predV v) ;
    ComplVQ v q = 
      insertExtrapos (comma ++ q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertAdj (ap.s ! APred) ap.c ap.ext (predV v) ;

    SlashV2a v = (predVc v) ; 
      
    Slash2V3 v np = insertObjNP np v.c2 (predVc v) ** {c2 = v.c3} ; 
    Slash3V3 v np = insertObjNP np v.c3 (predVc v) ; 

    SlashV2S v s = 
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (predV v) ** {c2 = v.c2; objCtrl = False} ;
    SlashV2Q v q = 
      insertExtrapos (comma ++ q.s ! QIndir) (predV v) ** {c2 = v.c2; objCtrl = False} ;
{-
    SlashV2V v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = v.c2} ;
      in vps ** 
       insertExtrapos vpi.p4 ( -- inplace vp; better extract it!
        insertInfExt vpi.p3 (
          insertInf vpi.p2 (
            insertObjc vpi.p1 vps))) ;

-}
    SlashV2V v vp =  -- (jmdn) bitten, sich zu waschen | sich waschen lassen  HL 7/19
      let
        vps = predVGen v.isAux v ; -- e.g. verspricht|bittet.isAux=False | läßt.isAux=True
        inf = mkInf v.isAux Simul Pos vp
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;

    SlashV2A v ap =
      insertAdj (ap.s ! APred) ap.c ap.ext (predV v) ** {c2 = v.c2; objCtrl = False} ;

    ComplSlash vps np =
      -- IL 24/04/2018 force reflexive in the VPSlash to take the agreement of np.
      -- HL 3/22 better before inserting np, using objCtrl
      let vp = case vps.objCtrl of { True => objAgr np vps ; _  => vps }
               ** { c2 = vps.c2 ; objCtrl = vps.objCtrl } ;
      in insertObjNP np vps.c2 vp ;

{-
    SlashVV v vp = 
      let
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2 } ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 vps)) ;
-}

    -- SlashVV v vps is like ComplVV v vp, but infinite vps should not be extracted
    SlashVV v vp =                                                    --  HL 3/2022
      let
        vps = predVGen v.isAux v ; -- e.g. will.isAux=True | wagt.isAux=False
        vpi = infVPSlash v.isAux Simul Pos vp ;     -- differs from infVP !
        inf : {inpl: (Agr => Str) * Str ; extr : (Agr => Str)} =
          let
            topInpl = <vpi.objs, vpi.pred> ;
            emptyInpl : (Agr => Str) * Str = <\\_ => [], []> ;
          in
          case <v.isAux,vp.isAux> of {
            <False,True> -- wagt lesen zu wollen
                => {inpl = emptyInpl ;
                    extr = let moved = (embedInf vpi.inpl topInpl)
                           in \\agr => (glueInpl moved)!agr ++ (vpi.extr!agr)} ;
              _ => -- wagt zu lesen zu versuchen
                   -- will lesen können | will zu lesen wagen
                   {inpl = embedInf vpi.inpl topInpl ; extr = vpi.extr}
            } ;
      in
      insertExtrapos vp.ext (
        insertInf inf vps) ** {c2 = vp.c2 ; objCtrl = vp.objCtrl};

{-  -- order of embedded objects wrong: 
    -- Lang> p "the woman that you beg me to listen to" | l
    --   the woman that you beg me to listen to
    --   die Frau , der ihr mich zuzuhören bittet
    -- better: die Frau , der zuzuhören ihr mich bittet 

    SlashV2VNP v np vp =
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2} ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 (
            insertObj (\\_ => appPrepNP v.c2 np) vps))) ;

-}
   -- expensive: + SlashV2VNP 503.884.800 (2880,540), reaches memory limit with SlashVV
   -- does not work for nested uses: the nn-levels are confused  HL 3/22

   SlashV2VNP v np vp =   -- bitte ihn, zu kaufen | lasse ihn kaufen   HL 3/22
      insertObjNP np v.c2 (ComplVV v vp ** {c2 = vp.c2 ; objCtrl = vp.objCtrl}) ;


    UseComp comp =
       insertExtrapos comp.ext (insertObj comp.s (predV sein_V)) ; -- agr not used
    -- adj slot not used here for e.g. "ich bin alt" but same behaviour as NPs?
	-- "ich bin nicht alt" "ich bin nicht Doris"

    UseCopula = predV sein_V ;

    CompAP ap = {s = \\_ => ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ; ext = ap.ext} ;
    CompNP np = {s = \\_ => np.s ! NPC Nom ++ np.rc ; ext = np.ext} ;
    CompAdv a = {s = \\_ => a.s ; ext = []} ;

    CompCN cn = {s = \\a => case numberAgr a of {
        Sg => "ein" + pronEnding ! GSg cn.g ! Nom ++ cn.s ! Strong ! Sg ! Nom ++ cn.rc ! Sg ; ---
        Pl => cn.s ! Strong ! Pl ! Nom ++ cn.rc ! Pl ---
        } ;
		ext = cn.adv ++ cn.ext
      } ;

    AdvVP vp adv = insertAdv adv.s vp ;
    ExtAdvVP vp adv = insertAdv (embedInCommas adv.s) vp ;

    AdVVP adv vp = insertAdv adv.s vp ; -- not AdV 27/5/2012: nicht immer

    AdvVPSlash vp adv = vp ** insertAdv adv.s vp ;
    AdVVPSlash adv vp = vp ** insertAdv adv.s vp ;

    -- ReflVP vp = insertObj (\\a => appPrep vp.c2 
    --   (\\k => usePrepC k (\c -> reflPron ! a ! c))) vp ;
    ReflVP vp = insertObjRefl vp ; -- HL, 19/06/2019

    PassV2 v = -- acc object -> nom subject; all others: same PCase
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c}
      in insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass) ** { c1 = v.c2 ** {c = c} } ;

{- HL: The construction VPSlashPrep : VP -> Prep -> VPSlash does not exist
   in German. In abstract/Verb.gf, the example

    VPSlashPrep : VP -> Prep -> VPSlash ;  -- live in (it)

   (with live_V:V) indicates that, here, you consider Prep=AdvSlash,
   so to speak, for building a compact version of relative clauses:

        the city we live in : NP

   from  "we live (in the city : Adv) : Cl"

   In German we cannot move the NP part of an Adv, we only have the
   full relative clauses like

        die Stadt, in der wir leben,
        die Stadt, worin wir leben,    --contracted Prep+Rel

   But: VPSlashPrep is used to parse "sie ist mit mir verheiratet", 
              (ist verheiratet:VP mit:Prep):VPSlash,
        ComplA2 is used to parse "sie ist verheiratet mit mir"
-}
    VPSlashPrep vp prep = vp ** {c2 = prep ; objCtrl = False} ;

}
