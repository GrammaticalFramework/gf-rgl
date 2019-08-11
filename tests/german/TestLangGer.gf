--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangGer of TestLang = 
  GrammarGer - [PassV2,SlashV2V,ComplVV,SlashVV] -- to improve these
  , TestLexiconGer
--  , ConstructionGer
  ** open ResGer,Prelude,(P=ParadigmsGer) in {

flags startcat = Phr ; unlexer = text ; lexer = text ;
  lincat
    VPSlashSlash = CatGer.VPSlash ** {c3 : Preposition} ;
  lin 
    ReflVPSlash v3 = (insertObjRefl (predVc v3) ** {c2 = v3.c3 ; missingAdv = True}); -- reflexive use of v:V3, untested

    -- SlashV3a v = (predVc v) ** {c3 = v.c3} ;

    Slash2V4 v np = (lin VPSlash (insertObjNP np v.c2 (predV v) ** {c2 = v.c3 ; missingAdv = True})) ** { c3 = v.c4 } ; 
    Slash3V4 v np = (lin VPSlash (insertObjNP np v.c3 (predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c4 } ;
    Slash4V4 v np = (lin VPSlash (insertObjNP np v.c4 (predV v) ** {c2 = v.c2 ; missingAdv = True})) ** { c3 = v.c3 } ;

--    ComplSlashSlash vpss np = insertObjNP np vpss.c2 vpss ** {c2 = vpss.c3 ; missingAdv = True } ;

    -- linref
    --   V4 = \v -> useInfVP False (predV v) ++ v.c2.s ++ v.c3.s ++ v.c4.s ;
{-
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
{- todo later:
    SlashVV v vp = 
      let 
        vpi = infVP v.isAux vp ;
        vps = predVGen v.isAux v ** {c2 = vp.c2 ; missingAdv = vp.missingAdv} ; -- mAdv added, HL 23.7.19
      in vps **                                                        -- to preserve reflexiveness of vp
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 vps)) ;
-}
    ComplVV v vp = -- will|wage (es (zu) tun [] | ihn [es tun] (zu) lassen | ihm (zu) versprechen [, es zu tun])
      let 
        vps = predVGen v.isAux v ;
        vpi = infzuVP v.isAux SubjC vp ;
        -- { objs: ihm ; pred: []/zu versprechen, objInf: sich/es zu tun }
        -- (ich) vfin:werde (ihm ([]/zu) versprechen) vinf:(wollen/gewagt haben) (, es zu tun)
        -- (ich) vfin:werde (ihn (es tun) lassen)/[]  vinf:(wollen/gewagt haben) []/(, ihn (es tun) zu lassen)
--        comma = case andB vp.isAux vp.inf.isAux of {True => [] ; _ => bindComma} ;
        comma = case vp.inf.ctrl of { NoC => [] ; _ => bindComma} ; -- es (zu) tun
        embeddedInf : Agr => Str = 
          case <vps.isAux,vp.isAux> of {<True,True> => -- <will,lassen> ihn [es tun] lassen 
                                          \\agr => "A[" ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ++ "]";           
                                        <False,True> => -- <wage,lassen> ihn [es tun] (zu) lassen 
                                          \\agr => "B[" ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ++ "]";           
                                        <True,False> => -- <will,versprechen> ihr versprechen [, es zu tun]
                                          \\agr => "C[" ++ comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ++ "]"; 
                                        <False,False> => -- [] | , es zu tun | (,) ihm (zu) versprechen , es zu tun
                                          \\agr => "D[" ++ comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 ++ "]"} 
      in
      insertExtrapos vpi.ext (         -- vps.ext   <- object-sentence of vp 
        insertInf vpi.pred (           -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs ( -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf embeddedInf vps))) ;
  lin
    SlashV2V v vp =   -- warnen, (\agr => sich!agr das Buch zu merken) 
      let 
        vps = (predVGen v.isAux v) ** { c2 = v.c2 ; 
                                        missingAdv = case v.ctrl of { SubjC => True ; _ => False } } ;
        vpi = infzuVP v.isAux v.ctrl vp ;
        comma = case orB vp.isAux (case vp.inf.ctrl of { NoC => True ; _ => False }) of {True => [] ; _ => bindComma} 
      in { missingAdv = True } **
      insertExtrapos vpi.ext (           -- vps.ext   <- object-sentence of vp 
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs (   -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf (\\agr => comma ++ (vp.nn!agr).p6 ++ (vp.nn!agr).p5 ++ vpi.inf) vps))) ;

  oper
    infzuVP : Bool -> Control -> ResGer.VP -> { objs:(Agr => Str) ; pred:{s:Str;isAux:Bool;ctrl:Control} ; inf:Str ; ext:Str } = 
      \isAux, ctrl, vp -> let vps = useVP vp in
      { pred = { s = vp.a1 ! Pos ++ vp.adj ++ (vps.s ! (notB isAux) ! agrP3 Sg ! VPInfinit Simul).inf ;
                 isAux = vp.isAux ; ctrl = ctrl } ;
        objs = \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 
          ++ vp.a2 ++ (vp.nn ! agr).p4 ;  -- objects + predicative A|CN|NP
        inf = vp.inf.s ; 
        ext = vp.ext  
      } ;

    insertInfExtraInf : (Agr => Str) -> VPSlash -> VPSlash = \inf,vp -> vp ** { 
      nn = \\a => let vpnn = vp.nn ! a in
        <vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4, vpnn.p5, vpnn.p6 ++ inf ! a>   
        -- embedded inf + modalInf / nonmodalInf + embedded inf ?
      } ;

{-
    SlashV2V v vp =   -- warne (\agr => sich!agr das Buch zu merken) : der infzu muss in nn.p4 oder ext:Agr=>Str
      let 
        b : Bool = case v.ctrl of { SubjC => True ; _ => False } ;
        vps = (predVGen v.isAux v) ** { c2 = v.c2 ; missingAdv = b } ; 
        vpi = infVP v.isAux vp ;
      in { missingAdv = True } **
       insertExtrapos vpi.p4 ( -- inplace vp; better extract it (in mkClause?) !
        insertInfExt vpi.p3 (
          insertInf vpi.p2 (
            insertObjc vpi.p1 vps))) ; -- glues all vp.nn-fields into nn.p4

   infVP : Bool -> VP -> ((Agr => Str) * Str * Str * Str) = \isAux, vp -> let vps = useVP vp in
    <
     \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ++ (vp.nn ! agr).p4 ++ vp.a2,
     vp.a1 ! Pos ++ vp.adj ++ (vps.s ! (notB isAux) ! agrP3 Sg ! VPInfinit Simul).inf,
     vp.inf,
     vp.infExt ++ vp.ext
    > ;
-}

}
