--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- --# -path=.:../abstract:../common:../api:../prelude

concrete TestLangGer of TestLang = 
  GrammarGer - [PassV2,SlashV2V,ComplVV,SlashVV] -- to improve these
  , TestLexiconGer
  , ConstructionGer
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

    ComplSlashSlash vpss np = insertObjNP np vpss.c2 vpss ** {c2 = vpss.c3 ; missingAdv = True } ;

    -- linref
    --   V4 = \v -> useInfVP False (predV v) ++ v.c2.s ++ v.c3.s ++ v.c4.s ;

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
        ++ vp.inf ++ vp.infExt ++ vp.s.s ! VPastPart af ;
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
    ComplVV v vp = 
      let 
        vps = predVGen v.isAux v ;
               -- ComplVV will/wage (es ihr []/zu verkaufen) = (vp.nn=<es,ihr>; vp.inf=verkaufen)
               --                   (ihm []/zu versprechen, (es ihr zu verkaufen)) = 
               --          v=wage   (vp.nn.p2=ihm; vp.inf=zu versprechen; vp.nn.p4=sich/es ihr zu verkaufen)
        infVP : Bool -> ResGer.VP -> {objNPs:(Agr => Str) ; objInf:(Agr => Str) ;
                                      pred:Str ; inf:Str ; ext:Str ; infExt:Str} = 
          \isAux, vp -> let vps = useVP vp in
          { objNPs = \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 
              ++ vp.a2 ++ (vp.nn ! agr).p4 ;  -- objs + predicative A|CN|NP
            objInf = \\agr => (vp.nn ! agr).p5 ;
            pred = vp.a1 ! Pos ++ vp.adj ++ (vps.s ! (notB isAux) ! agrP3 Sg ! VPInfinit Simul).inf ;
            inf = vp.inf ; ext = vp.ext ; infExt = vp.infExt 
          } ;
        comma = case v.isAux of {True => [] ; _ => bindComma} ;
        vpi = infVP v.isAux vp         
        -- plan:
        -- { objNPs: ihm ; pred: []/zu versprechen, objInf: sich/es ihr zu verkaufen }
        -- (ich) vfin:werde ihm ([]/zu) versprechen vinf:(wollen/gewagt haben) , (es ihr zu verkaufen)
        -- (ich) vfin:werde (ihn (es ihr verkaufen) lassen)/[] vinf:(wollen/gewagt haben) 
        --                  []/,(ihn (es ihr verkaufen) zu lassen)
      in
      insertExtrapos vpi.ext (           -- vps.ext   <- object-sentence of vp 
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objNPs ( -- vps.nn.p5 <- vp's object nps
          insertInfExtraInf (\\agr => (vp.nn!agr).p6 ++ (vp.nn!agr).p5 ++ vpi.inf) vps))) ;

  oper
  insertInfExtraInf : (Agr => Str) -> VPSlash -> VPSlash = \inf,vp -> vp ** { 
    nn = \\a => let vpnn = vp.nn ! a in
      <vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4, vpnn.p5, vpnn.p6 ++ inf ! a>   
      -- embedded inf + modalInf / nonmodalInf + embedded inf ?
    } ;
  lin
    SlashV2V v vp =   -- warne (\agr => sich!agr das Buch zu merken) : der infzu muss in nn.p4 oder ext:Agr=>Str
      let 
        b : Bool = case v.ctrl of { SubjC => True ; _ => False } ;
        vps = (predVGen v.isAux v) ** { c2 = v.c2 ; missingAdv = b } ; 
        infVP : Bool -> ResGer.VP -> {objNPs:(Agr => Str) ;
                                      pred:Str ; inf:Str ; ext:Str ; infExt:Str} = 
          \isAux, vp -> let vps = useVP vp in
          { objNPs = \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 
              ++ (vp.nn ! agr).p4 ++ vp.a2 ;
            pred = vp.a1 ! Pos ++ vp.adj ++ (vps.s ! (notB isAux) ! agrP3 Sg ! VPInfinit Simul).inf ;
            inf = vp.inf ; ext = vp.ext ; infExt = vp.infExt 
          } ;
        comma = case v.isAux of {True => [] ; _ => bindComma} ;
        vpi = infVP v.isAux vp 
      in { missingAdv = True } **
      insertExtrapos vpi.ext (           -- vps.ext   <- object-sentence of vp 
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objNPs ( -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf (\\agr => (vp.nn!agr).p6 ++ (vp.nn!agr).p5 ++ vpi.inf) vps))) ;

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
