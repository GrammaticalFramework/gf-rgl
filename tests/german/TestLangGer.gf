--# -path=.:../../src/abstract:../../src/common:../../src/api:../../src/prelude:../../src/german
-- use the modified files in gf-rgl/src/german

concrete TestLangGer of TestLang = 
  GrammarGer - [SlashVP, RelSlash]
  , TestLexiconGer
  , ConstructionGer
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

    PassV2Q v q =
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp = insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass)
            ** { c1 = v.c2 ** {c = c} }
      in insertExtrapos (bindComma ++ q.s ! QIndir) vp ;

    PassV2S v s =
      let c = case <v.c2.c, v.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp = insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass)
            ** { c1 = v.c2 ** {c = c} }
      in insertExtrapos (bindComma ++ conjThat ++ s.s ! Sub) vp ;

    PassV2V v vp = 
      let
          inf = mkInf v.isAux Simul Pos vp ;             -- ok for v.isAux=False, v.c2.c=Acc
          c = case <v.c2.c, v.c2.isPrep> of {            --        v.objCtrl=True   HL 3/22
            <NPC Acc, False> => NPC Nom ; _ => v.c2.c} ; -- acc;pcase object -> nom;pcase subject
          vp2 = insertObj (\\_ => v.s ! VPastPart APred) (predV werdenPass)
            ** { c1 = v.c2 ** {c = c} } ;
        in insertInf inf vp2 ;                           -- v=lassen needs in-place inf instead

    PassVPSlash vp = 
      let c = case <vp.c2.c,vp.c2.isPrep> of {
            <NPC Acc, False> => NPC Nom ; _ => vp.c2.c} ;
          ctrl = case vp.objCtrl of { True => False ; _ => True }  -- always False?
      in -- insertObj (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass ** {c1 = vp.c2 ** {c = c}})
          insertObj (\\_ => vp.s.s ! (VPastPart APred))
                      (predV werdenPass ** {nn = vp.nn ; c1 = vp.c2 ** {c = c}})
           ** {ext = vp.ext ; inf = vp.inf ; c2 =vp.c2 ; objCtrl = ctrl } ;  -- c2 ?
       -- Scharolta: passivised object: acc object -> nom subject; all others: same case/prep
       -- HL: does not work for vp = (Slash2V3 v np): uns wird *den Beweis erklärt
       -- 3/22 works for        vp = (SlashV2V v2v reflVP): wir werden gebeten, uns zu waschen

    PastPartAP vp = {
      s = \\af => (vp.nn ! agrP3 Sg).p1 ++ (vp.nn ! agrP3 Sg).p2 ++ 
        (vp.nn ! agrP3 Sg).p3 ++ (vp.nn ! agrP3 Sg).p4 ++ vp.adj ++ vp.a2 
        ++ vp.inf.inpl.p2 ++ vp.s.s ! VPastPart af ;
      isPre = True ;
      c = <[],[]> ;
      adj = [] ;
      ext = (vp.inf.extr ! agrP3 Sg) ++ vp.ext  -- HL 5/4/2022
      } ;

    Pass2V3 v np = -- HL 7/19: making the (active) direct object to the (passive) subject
      let vps = insertObj (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { c1 = PrepNom ; c2 = v.c3 }
      in insertObjNP np vps.c2 (vps ** {objCtrl = False});

    Pass3V3 v np = -- HL 7/19: making the (active) indirect object to the (passive) subject
      let bekommen : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") ;
          vps = insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommen)
            ** { c1 = PrepNom ; c2 = v.c2 }
      in insertObjNP np vps.c2 (vps ** {objCtrl = False});
{-      
    Pass2V4 v np = 
      let vps = -- : VPSlashSlash = 
            insertObj (\\_ => (v.s ! VPastPart APred)) (predV werdenPass)
            ** { c1 = PrepNom ; c2 = v.c3 ; c3 = v.c4 }
      in (insertObjNP np vps.c3 vps) ;

    -- Todo: Pass?V2S, Pass?V2Q, PassVS, PassVQ Pass?V2V
-}

    SlashV2Vneg v vp = -- HL 3/22
      let 
        vps = (predVGen v.isAux v) ;
        inf = mkInf v.isAux Simul Neg vp
       in
        insertExtrapos vp.ext (
          insertInf inf vps) ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;

-- SlashV2VNP : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
-- -- (the book) that (she (begged:V2V me:NP (to buy ()):VPSlash):VPSlash):ClSlash

-- 3/22 expensive: |NP|=54, |Prep|=|18|, |V2|=180, |V2V|=720, |VP|=360, |VPSlash|=12.290 (!)
-- i -v -src TestLangGer.gf
-- + ComplSlash     699840 (355680,532)
-- + SlashV2V       259200   (1440,20)
-- + SlashV2VNP  503884800  (77760,540)
-- 243273 msec

{-
TestLang> p "the book that we beg her to promise him to read" | l
the book that we beg her to promise him to read
das Buch , das wir sie bitten , ihm zu versprechen , zu lesen

TestLang> p "the book that we beg her to beg him to read" | l
the book that we beg her to beg him to read
das Buch , das wir sie bitten , ihn zu bitten , zu lesen

TestLang: DetCN (DetQuant DefArt NumSg) (RelCN (UseN book_N) (UseRCl (TTAnt TPres ASimul) PPos (RelSlash IdRP (SlashVP (UsePron we_Pron) (SlashV2VNP versprechen_dat_V2V (UsePron she_Pron) (SlashV2a read_V2))))))
TestLangEng: the book that we promise her to read
TestLangGer: das Buch , das wir ihr versprechen , zu lesen

TestLang> p "the book that we beg her to sell to him" | l
the book that we beg her to sell to him
das Buch , das wir sie bitten , ihm zu verkaufen

p -lang=Ger "das Kind , auf das wir ihn bitten , zu warten ," | l
the child that we beg him to wait for
das Kind , auf das wir ihn bitten , zu warten

pied piping:
~~> das Buch , das ihm zu verkaufen wir sie bitten
~~> das Kind , auf das zu warten wir ihn bitten

Wrong in gf-3.9 as well:
Lang> p "the woman that we beg him to listen to" | l
the woman that we beg him to listen to
die Frau , der wir ihn bitten , zuzuhören

Lang> p "the book that we beg her to sell to him" | l
the book that we beg her to sell to him
das Buch, das wir sie bitten, ihm zu verkaufen

Test reflexive resolution:

gr -tr (PredVP (UsePron ?) (ComplSlash (SlashV2V lassen_V2V (ReflVP (SlashV2a wash_V2))) (UsePron ?))) | l

-}

--  Reimplementation of SlashVP: replace mkClause by mkClSlash to let reflexives agree
--  with object in relative clauses, if objCtrl = true. To save memory, use
--  ClauseSlash.s : ... => RelGenNum => Str instead of : ... => Agr => Str.
-- + SlashVP 699840 gf: out of memory (requested 2097152 bytes) with Agr => Str
-- + SlashVP 699840 (2880,160) with  RelGenNum => Str; |Agr| = 18, |RelGenNum| = 5.
-- Todo: replace other uses of ClSlash by ClauseSlash

  lincat
    ClauseSlash = {
      s : Mood => ResGer.Tense => Anteriority => Polarity => Order => RelGenNum => Str ;
      c2 : Preposition
      } ;

  lin
    SlashVP np vp =
      let subj = mkSubj np vp.c1
      in mkClSlash subj.p1 subj.p2 vp ** { c2 = vp.c2 } ;

    RelSlash rp cls = lin RCl {
      s = \\m,t,a,p,gn =>
          appPrep cls.c2 (\\k => usePrepC k (\c -> rp.s ! gn ! c)) ++
          cls.s ! m ! t ! a ! p ! Sub ! gn ;
      c = (prepC cls.c2.c).c
      } ;
{-
    QuestSlash ip slash = {
      s = \\m,t,a,p => 
            let 
              cls = slash.s ! m ! t ! a ! p ;
              who = appPrep slash.c2 (\\k => usePrepC k (\c -> ip.s ! c)) ;
            in table {
              QDir   => who ++ cls ! Inv ;
              QIndir => who ++ cls ! Sub
              }
      } ;
-}


  oper
    gnToAgr : RelGenNum -> Agr = \gn ->
       case gn of {RGenNum (GSg g) => Ag g Sg P3 ;
                   RGenNum GPl     => Ag Neutr Pl P3 ;
                   RSentence       => Ag Neutr Sg P3} ;


    mkClSlash : Str -> Agr -> ResGer.VPSlash -> ClauseSlash = \subj,agr,vp ->
    let vps = useVP vp in lin ClauseSlash {
      c2 = vp.c2 ;
      s = \\m,t,a,b,o,gn =>
        let
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          verb  = vps.s  ! ord ! agr ! VPFinite m t a ;
          haben = verb.inf2 ;
          neg = negation ! b ;
          ag : Agr = case vp.objCtrl of {True => gnToAgr gn ; _ => agr} ;
          obj1  = (vp.nn ! ag).p1 ++ (vp.nn ! ag).p2 ; -- refl ++ pronouns ++ light nps
          obj2  = (vp.nn ! ag).p3 ;                    -- pp-objects and heavy nps
          obj3  = (vp.nn ! ag).p4 ++ vp.adj ++ vp.a2 ; -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl : Str = obj1 ++ obj2 ++ neg ++ obj3 ;
          infObjs = (vp.inf.inpl.p1) ! ag ;
          infPred = vp.inf.inpl.p2 ;
          infCompl : Str = case <t,a,vp.isAux> of {
                 <Fut|Cond,Anter,True> => [] ;  _ => infObjs ++ infPred } ;
          pred : {inf, infComplfin : Str} = case <t,a,vp.isAux> of {
             <Fut|Cond,Anter,True>  =>                                    --# notpresent
               {inf    = infObjs ++ haben ++ infPred ++ verb.inf ;        --# notpresent Duden 318
                infComplfin = -- es ++ wird ++ haben ++ tun ++ wollen     --# notpresent
                   infObjs ++ verb.fin ++ haben ++ infPred ++ verb.inf} ; --# notpresent
             <_,Anter,True> =>                                            --# notpresent
               {inf    = verb.inf ++ haben ;                              --# notpresent
                infComplfin = -- es ++ wird/hat/hatte ++ tun ++ wollen    --# notpresent
                   infObjs ++ verb.fin ++ infPred ++ verb.inf ++ haben} ; --# notpresent
              <Pres,_,_> =>
               {inf    = verb.inf ++ haben ;
                infComplfin = -- es zu tun ++ [] ++ [] ++ versucht
                   infCompl ++ verb.inf ++ haben ++ verb.fin}
                                                                         ; --# notpresent
              _ =>                                                         --# notpresent
               {inf    = verb.inf ++ haben ;                               --# notpresent
                infComplfin = -- es zu tun ++ versucht ++ [] ++ hat        --# notpresent
                              infCompl ++ verb.inf ++ haben ++ verb.fin}   --# notpresent
              } ;
          extra : Str = (vp.inf.extr) ! ag ++ vp.ext ;
        in
        case o of {
	  Main => subj ++ verb.fin ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Inv  => verb.fin ++ subj ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Subj =>             subj ++ compl ++   pred.infComplfin   ++ extra
        }
    } ;

}

