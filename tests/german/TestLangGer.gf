--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- use the modified files in gf-rgl/src/german

concrete TestLangGer of TestLang = 
  GrammarGer - [PassV2] -- to improve these ,ComplVV,SlashVV,SlashV2V,SlashV2VNP
  , TestLexiconGer - [helfen_V2V, warnen_V2V, versprechen_dat_V2V, lassen_V2V]
--  , ConstructionGer -- needs SlashV2VNP of VerbGer
  ** open ResGer,Prelude,(P=ParadigmsGer) in {

  flags startcat = Phr ; unlexer = text ; lexer = text ;
        optimize=all_subs ;
{-
  lincat
    VPSlashSlash = CatGer.VPSlash ** {c3 : Preposition} ;
  lin 
    SlashV3a v = (predVc v) ** {c3 = v.c3} ;

    Slash2V4 v np = insertObjNP np v.c2 (predV v) ** {c2 = v.c3 ; c3 = v.c4 } ; 
    Slash3V4 v np = insertObjNP np v.c3 (predV v) ** {c2 = v.c2 ; c3 = v.c4 } ;
    Slash4V4 v np = insertObjNP np v.c4 (predV v) ** {c2 = v.c2 ; c3 = v.c3 } ;
    
    ComplSlashSlash vpss np = insertObjNP np vpss.c2 vpss ** {c2 = vpss.c3} ;
    -- linref
    --   V4 = \v -> useInfVP False (predV v) ++ v.c2.s ++ v.c3.s ++ v.c4.s ;
-}
  lin
    ReflVPSlash v3 = -- reflexive use of v:V3, untested
      (insertObjRefl (predVc v3) ** {c2 = v3.c3}); 

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
{-
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
-}
    Pass2V3 v np = -- HL 7/19: making the (active) direct object to the (passive) subject
      let vps = insertObjc (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { subjc = PrepNom ; c2 = v.c3 }
      in insertObjNP np vps.c2 vps ;

    Pass3V3 v np = -- HL 7/19: making the (active) indirect object to the (passive) subject
      let bekommen : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") ;
          vps = insertObjc (\\_ => (v.s ! VPastPart APred)) (predV bekommen)
            ** { subjc = PrepNom ; c2 = v.c2 } 
      in insertObjNP np vps.c2 vps ;
{-      
    Pass2V4 v np = 
      let vps = -- : VPSlashSlash = 
            insertObj (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { subjc = PrepNom ; c2 = v.c3 ; c3 = v.c4 } 
      in (insertObjNP np vps.c3 vps) ;

    -- Todo: Pass?V2S, Pass?V2Q, PassVS, PassVQ Pass?V2V
-}

    SlashV2Vneg v vp =   -- versprechen, (\agr => sich!agr es nicht zu merken) 
      let 
        vps = (predVGen v.isAux v) ** { c2 = v.c2 } ; --; ctrl = v.ctrl } ;
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

    lin -- with param Control in ../../src/german/ParadigmsGer.gf
      helfen_V2V = P.mkV2V (P.irregV "helfen" "hilft" "half" "hälfe" "geholfen") P.datPrep ; 
      warnen_V2V = P.mkV2V (P.regV "warnen") P.accPrep ;
      versprechen_dat_V2V = 
        P.subjV2V (P.mkV2V (P.irregV "versprechen" "verspricht" "versprach" "verspräche" "versprochen") P.datPrep) ;
      lassen_V2V = P.auxV2V (P.irregV "lassen" "lasst" "ließ" "ließe" "gelassen") P.accPrep ;  -- lasse dich (*zu) arbeiten

-- SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
-- -- (the book) that (she (begged:V2V me:NP (to buy ()):VPSlash):VPSlash):ClSlash

-- very expensive:
-- + SlashV2V 2332800 (6480,40)
-- + SlashV2VNP 2267481600 (4320,270) vs. (1080,90) in VerbGer, 305460 msec
-- Languages: TestLangGer
-- 623657 msec
{- 
    SlashV2VNP v np vp =  
      let
        vps = (predVGen v.isAux v) ** { c2 = vp.c2 } ; -- objCtrl = 
        vpi = infzuVP v.isAux v.ctrl Simul Pos vp ;
        -- comma = case <vp.isAux,vp.inf.ctrl> of { <True,_> => [] ; <_,NoC> => [] ; _ => bindComma} ;
        embeddedInf : Agr => Str = 
          \\agr => "[" ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ++ "]";  
        -- embeddedInf : Agr => Str = case vp.inf.isAux of {
        --   True  => \\agr => comma ++ (vp.nn!agr).p5 ++ (vp.nn!agr).p6 ++ vpi.inf ;  -- ihn es lesen (zu) lassen
        --   False => \\agr => comma ++ (vp.nn!agr).p5 ++ vpi.inf ++ (vp.nn!agr).p6 }  -- ihn (zu) bitten , es zu lesen
      in
      insertExtrapos vpi.ext (           -- vps.ext   <- vp's object-sentence ++ extractedInfzu?
        insertInf vpi.pred (             -- vps.inf   <- vp's infinite main verb
          insertInfExtraObj vpi.objs (   -- vps.nn.p5 <- vp's object nps
            insertInfExtraInf embeddedInf (
              insertObjNP np v.c2 vps )))) ;
-}
{-
TestLang> p "the book that we beg her to promise him to read" | l
the book that we beg her to promise him to read
das Buch , das wir sie bitten , ihn zu versprechen [ [ ] zu lesen ]

TestLang> p "the book that we beg her to beg him to read" | l
the book that we beg her to beg him to read
das Buch , das wir sie bitten , ihn zu bitten [ [ ] zu lesen ]

TestLang: DetCN (DetQuant DefArt NumSg) (RelCN (UseN book_N) (UseRCl (TTAnt TPres ASimul) PPos (RelSlash IdRP (SlashVP (UsePron we_Pron) (SlashV2VNP versprechen_dat_V2V (UsePron she_Pron) (SlashV2a read_V2))))))
TestLangEng: the book that we promise her to read
TestLangGer: das Buch , dem wir ihr versprechen , zu lesen   Bug: dem => das

TestLang> p "the book that we beg her to sell to him" | l
the book that we beg her to sell to him
das Buch , das wir ihm sie bitten , zu verkaufen
=> das Buch , das wir sie bitten , ihm zu verkaufen
~~> das Buch , das ihm zu verkaufen wir sie bitten
 ~~ das Buch , an das zu glauben wir sie bitten

Wrong in gf-3.9 as well:
Lang> p "the woman that we beg him to listen to" | l
the woman that we beg him to listen to
die Frau , die wir ihn zuzuhören bitten  (Bug: die => der)

Lang> p "the book that we beg her to sell to him" | l
the book that we beg her to sell to him
das Buch , das wir ihn sie zu verkaufen bitten (Bug: ihn sie => sie ihm)
=> das Buch, das wir sie bitten, ihm zu verkaufen
-}
}
