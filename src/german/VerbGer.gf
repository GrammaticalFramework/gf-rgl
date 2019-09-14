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
    -- HL 7/19
    ComplVV v vp = -- will|wage (es ([]|zu) tun [] | ihn [es tun] ([]|zu) lassen
      let
        vps = predVGen v.isAux v ;
        vpi = infzuVP v.isAux SubjC Simul Pos vp ;
        -- { objs: ihm ; pred: []/zu versprechen, objInf: sich/es zu tun }
        -- (ich) vfin:werde (ihm ([]/zu) versprechen) vinf:(wollen/gewagt haben) (, es zu tun)
        -- (ich) vfin:werde (ihn (es tun) lassen)/[]  vinf:(wollen/gewagt haben) []/(, ihn (es tun) zu lassen)
        extInfzu = case <vp.isAux,vp.inf.isAux> of {<True,False> => (vp.nn!(Ag Masc Sg P3)).p6 ; _ => []} ;
        comma = case vp.inf.ctrl of { NoC => [] ; _ => bindComma} ; -- es (zu) tun
        embeddedInf : Agr => Str =
          case <vp.isAux,vp.inf.isAux> of { -- vv + vp + [embeddedInf]
            -- will [es lesen] können | will ihn [es lesen] lassen
            <True,True>  => \\agr => (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;
            -- will ihn [euch (extInfzu) bitten] lassen
            <True,False> => \\agr => (vp.nn!agr).p5 ++ vpi.inf ; -- ++ (vp.nn!agr).p6 => extInfzu
            -- will es lesen [] | will ihn bitten [, es zu lesen] | will ihn bitten [, sie es lesen zu lassen]
            <False,True>  => \\agr => comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;
            -- will ihn bitten [, ihr zu helfen, es zu lesen]
            <False,False> => \\agr => comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 }
      in
      insertExtrapos (extInfzu ++ vpi.ext) ( -- vps.ext   <- vp's extracted embedded infzu + vp's object-sentence
        insertInf vpi.pred (                 -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs (       -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf embeddedInf vps))) ;

    ComplVS v s = 
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (predV v) ;
    ComplVQ v q = 
      insertExtrapos (comma ++ q.s ! QIndir) (predV v) ;
    ComplVA v ap = insertAdj (v.c2.s ++ ap.s ! APred) ap.c ap.ext (predV v) ; -- changed

    SlashV2a v = (predVc v) ; 
      
    Slash2V3 v np = insertObjNP np v.c2 (predVc v) ** {c2 = v.c3} ; 
    Slash3V3 v np = insertObjNP np v.c3 (predVc v) ; 

    SlashV2S v s = 
      insertExtrapos (comma ++ conjThat ++ s.s ! Sub) (predVc v) ;
    SlashV2Q v q = 
      insertExtrapos (comma ++ q.s ! QIndir) (predVc v) ;
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
    SlashV2V v vp =   -- jmdn bitten, (\agr => sich!agr das Buch zu merken)  HL 7/19
      let
        vps = (predVGen v.isAux v) ** { c2 = v.c2 ; objCtrl = case v.ctrl of {ObjC => True ; _ => False}} ;
        vpi = infzuVP v.isAux v.ctrl Simul Pos vp ;
        comma : Str = case <vp.isAux,vp.inf.ctrl> of { <True,_> | <_,NoC> => [] ; _ => bindComma} ;
        embeddedInf : Agr => Str = case vp.inf.isAux of {
          True  => \\agr => comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;  -- ihn es lesen (zu) lassen
          False => \\agr => comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 }  -- ihn (zu) bitten , es zu lesen
      in
      insertExtrapos vpi.ext (           -- vps.ext   <- vp's object-sentence ++ extractedInfzu?
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs (   -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf embeddedInf vps))) ;

    SlashV2A v ap =
      insertAdj (ap.s ! APred) ap.c ap.ext (predVc v) ;

    ComplSlash vps np =
      let vp = insertObjNP np vps.c2 vps ;
          -- IL 24/04/2018 force reflexive in the VPSlash to take the agreement of np.
      in case vps.objCtrl of { True => objAgr np vp ; _  => vp } ;
{-
    SlashVV v vp = 
      let
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2 } ;
      in vps **
      insertExtrapos vpi.p3 (
        insertInf {s=vpi.p2;isAux=vp.isAux;ctrl=SubjC} ( -- insertInf vpi.p2 (
          insertObj vpi.p1 vps)) ;
-}
    SlashVV v vp = -- will|hoffe ((zu) lesen | ihr (zu) geben | (zu) bitten, es zu lesen) 
      let
        vps = (predVGen v.isAux v) ** { c2 = vp.c2 } ;
        vpi = infzuVP v.isAux SubjC Simul Pos vp ; -- (zu) (lesen | ihr geben | bitten, es zu lesen) 
        comma : Str = case <vp.isAux,vp.inf.ctrl> of { <True,_> | <_,NoC> => [] ; _ => bindComma} ;
        embeddedInf : Agr => Str = case vp.inf.isAux of {
          True  => \\agr => comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;  -- es lesen (zu) lassen
          False => \\agr => comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 }  -- (zu) bitten, es zu lesen
      in
      insertExtrapos vpi.ext (           -- vps.ext   <- vp's object-sentence ++ extractedInfzu?
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs (   -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf embeddedInf vps))) ;

-- {- HL 8/19: this slightly modified SlashV2VNP is expensive even with NP.w:Weight 

    -- order of embedded objects wrong: 
    -- Lang> p "the woman that you beg me to listen to" | l
    --   the woman that you beg me to listen to
    --   die Frau , der ihr mich zuzuhören bittet
    -- better: die Frau , der zuzuhören ihr mich bittet 

    SlashV2VNP v np vp =
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2} ; -- objCtrl = ?
      in vps **
      insertExtrapos vpi.p3 (
        insertInf {s=vpi.p2;isAux=v.isAux;ctrl=v.ctrl} ( -- insertInf vpi.p2
          insertObj vpi.p1 (
            insertObj (\\_ => appPrepNP v.c2 np) vps))) ;

    -- HL: version with infzuVP in tests/german/TestLangGer.gf, too expensive

    UseComp comp =
       insertExtrapos comp.ext (insertObj comp.s (predV sein_V)) ; -- agr not used
    -- adj slot not used here for e.g. "ich bin alt" but same behaviour as NPs?
	-- "ich bin nicht alt" "ich bin nicht Doris" 

    UseCopula = predV sein_V ;

    CompAP ap = {s = \\_ => ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ; ext = ap.ext} ;
    CompNP np = {s = \\_ => np.s ! NPC Nom ++ np.rc ; ext = np.ext} ;
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
