--# -path=../common:../abstract:../common

concrete ExtendRus of Extend =
  CatRus ** ExtendFunctor - [
    iFem_Pron,
    -- theyFem_Pron, weFem_Pron,
    youFem_Pron,
    -- VPS, ListVPS, VPI, ListVPI, VPS2, ListVPS2, VPI2, ListVPI2, RNP, RNPList,
    -- UseComp, RelNP, UseComp_estar, SubjRelNP, PredAPVP, EmbedVP,
    -- ExistNP, ExistIP, AdvVP, AdvVP, AdvVP, ExistS, ExistNPQS, ExistIPQS,
    --
    ComplDirectVS,
    ComplDirectVQ,
    -- AdvIsNPAP, AdAdV, AdjAsNP,
    ApposNP,
    BaseVPS, ConsVPS,
    -- BaseVPI, ConsVPI, BaseVPS2, ConsVPS2, BaseVPI2, ConsVPI2,
    MkVPS,
    ConjVPS,
    -- MkVPI, ConjVPI, ComplVPIVV,
    -- MkVPS2, ConjVPS2, ComplVPS2, MkVPI2, ConjVPI2, ComplVPI2,
    -- Base_nr_RNP, Base_rn_RNP, Base_rr_RNP, ByVP, CompBareCN,
    -- CompQS, CompS, CompVP, ComplBareVS, ComplGenVV, ComplSlashPartLast, ComplVPSVV, CompoundAP,
    CompoundN, CompoundAP,

    ConjImp, BaseImp, ConsImp,
    RNP, RNPList, ConjRNP, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP,
    DetNPMasc,
    DetNPFem,
    UseDAP,
    UseDAPMasc,
    UseDAPFem,
    -- EmbedPresPart, EmptyRelSlash,
    ExistsNP,
    -- ExistCN, ExistMassCN, ExistPluralCN,
    --ProDrop,
    -- FocusAP, FocusAdV, FocusAdv,
    FocusObj,
    -- GenIP, GenModIP, GenModNP, GenNP, GenRP,
    GerundAdv, GerundCN, GerundNP, ByVP,
    InOrderToVP,
    -- NominalizeVPSlashNP,
    PassAgentVPSlash,
    PassVPSlash,
    ProgrVPSlash,
    PastPartAP,
    PastPartAgentAP,
    PositAdVAdj,
    PredVPS,
    -- PredVPSVV, PredetRNP, PrepCN,
    PresPartAP,
    PurposeVP,
    ReflPoss, ReflPron, ReflRNP, PredetRNP, AdvRNP, AdvRVP, AdvRAP,
    ReflA2RNP, PossPronRNP,
    PredIAdvVP,
    -- UncontractedNeg, UttAccIP, UttAccNP,
    FrontComplDirectVS,
    FrontComplDirectVQ,
    UttAdV
    -- UttDatIP, UttDatNP, UttVPShort, WithoutVP
   ]
  with (Grammar=GrammarRus)
  ** open Prelude, ResRus, ParadigmsRus, TenseRus, Coordination, (M = MorphoRus) in {

lincat
  VPS     = {s : Mood => Agr => Str} ;
  [VPS]  = {s1,s2 : Mood => Agr => Str} ;
  [Comp] = {s1,s2 : AgrTable ; cop : CopulaType} ;
  [Imp] = {s1,s2 : Polarity => GenNum => Str} ;
  RNP     = {s : Case => Str} ;
  RNPList = {s1,s2 : Case => Str} ;

lin
  -- : NP -> NP -> NP ;        -- Mr Macron, the president of France,
  ApposNP np np2 = {
    s=\\cas=> np.s ! cas ++ embedInCommas (np2.s ! cas) ;
    pron=False ;
    a=np.a
    } ;

  -- : IAdv -> VP -> QCl ; -- how to walk?
  PredIAdvVP iadv vp = QuestIAdv iadv (GenericCl vp) ; -- DEFAULT how does one walk

  -- : VP -> Adv ;         -- (in order) to publish the document
  InOrderToVP vp = {
    s = "чтобы"
      ++ vp.adv ! Ag (GSg Neut) P3
      ++ (verbInf vp.verb)
      ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3
    } ;

  -- : VP -> Adv ;  -- to become happy
  PurposeVP vp = {
    s = vp.adv ! Ag (GSg Neut) P3 ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! Ag (GSg Neut) P3
    } ;

  -- : NP -> Cl ;  -- there exists a number / there exist numbers
  ExistsNP np = {
    subj=[] ;
    adv=[] ;
    verb=copulaFull ;
    dep=[] ;
    compl=table {
      Pos => np.s ! Nom ;
      Neg => np.s ! Gen
      } ;
    a=np.a
    } ;



  iFem_Pron = personalPron (Ag (GSg Fem) P1) ;
  youFem_Pron = personalPron (Ag (GSg Fem) P2) ;

  -- : N -> N -> N ;
  -- CompoundN n1 n2 = mkCompoundN n1 n2 ;

  CompoundN n1 n2 = case n1.rt of {
                          GenType => n2 ** {snom = n2.snom ++ n1.sgen ;
                                            sgen = n2.sgen ++ n1.sgen ;
                                            sdat = n2.sdat ++ n1.sgen;
                                            sacc = n2.sacc ++ n1.sgen;
                                            sins = n2.sins ++ n1.sgen;
                                            sprep = n2.sprep ++ n1.sgen;
                                            sloc = n2.sloc ++ n1.sgen;
                                            sptv = n2.sptv ++ n1.sgen;
                                            svoc = n2.svoc ++ n1.sgen;
                                            pnom = n2.pnom ++ n1.sgen;
                                            pgen = n2.pgen ++ n1.sgen;
                                            pdat = n2.pdat ++ n1.sgen;
                                            pacc = n2.pacc ++ n1.sgen;
                                            pins = n2.pins ++ n1.sgen;
                                            pprep = n2.pprep ++ n1.sgen;
                                            } ;
                          AdjType => n2 ** {snom = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Nom ++ n2.snom;
                                            sgen = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Gen ++ n2.sgen ;
                                            sdat = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Dat ++ n2.sdat ;
                                            sacc = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Acc ++ n2.sacc ;
                                            sins = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Ins ++ n2.sins ;
                                            sprep = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Loc ++ n2.sprep ;
                                            sloc = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Loc ++ n2.sloc ;
                                            sptv = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Gen ++ n2.sptv ;
                                            svoc = (adjFormsAdjective n1.rel).s ! (gennum n2.g Sg) ! n2.anim ! Nom ++ n2.svoc ;
                                            pnom = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Nom ++ n2.pnom ;
                                            pgen = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Gen ++ n2.pgen ;
                                            pdat = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Dat ++ n2.pdat ;
                                            pacc = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Acc ++ n2.pacc ;
                                            pins = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Ins ++ n2.pins ;
                                            pprep = (adjFormsAdjective n1.rel).s ! (gennum n2.g Pl) ! n2.anim ! Loc ++ n2.pprep ;
                                            }
                        } ;

  -- : VP -> AP ; -- (the man) looking at Mary
  PresPartAP vp = {
      s=\\gn,anim,cas =>
        vp.adv ! (genNumAgrP3 gn)
        ++ (presActPart vp.verb).s ! gn ! anim ! cas
        ++ vp.dep
        ++ vp.compl ! Pos ! (genNumAgrP3 gn) ;
      short=\\gn =>
        vp.adv ! (genNumAgrP3 gn)
        ++ (presActPart vp.verb).s ! gn ! Animate ! Nom
        ++ vp.dep
        ++ vp.compl ! Pos ! (genNumAgrP3 gn) ;
      isPost=False ;
      preferShort=PreferFull
      } ;

  -- VPSlash -> AP ; -- lost (opportunity) ; (opportunity) lost in space
  PastPartAP vps = {
    s=\\gn,anim,cas =>
      vps.adv ! (genNumAgrP3 gn)
      ++ (pastPassPart vps.verb).s ! gn ! anim ! cas
      ++ vps.dep
      ++ vps.compl1 ! Pos ! (genNumAgrP3 gn)
      ++ vps.compl2 ! Pos ! (genNumAgrP3 gn);
    short=\\gn =>
      vps.adv ! (genNumAgrP3 gn)
      ++ (pastPassPart vps.verb).short ! gn
      ++ vps.dep
      ++ vps.compl1 ! Pos ! (genNumAgrP3 gn)
      ++ vps.compl2 ! Pos ! (genNumAgrP3 gn) ;
    isPost = case vps.isSimple of {
               True  => False ;
               False => True
             } ;
    preferShort=PreferFull
    } ;

  -- : VPSlash -> NP -> AP ;   -- (opportunity) lost by the company
  PastPartAgentAP vps np = {
    s=\\gn,anim,cas =>
      vps.adv ! (genNumAgrP3 gn)
      ++ (pastPassPart vps.verb).s ! gn ! anim ! cas
      ++ vps.dep
      ++ np.s ! Ins
      ++ vps.compl1 ! Pos ! (genNumAgrP3 gn)
      ++ vps.compl2 ! Pos ! (genNumAgrP3 gn);
    short=\\gn =>
      vps.adv ! (genNumAgrP3 gn)
      ++ (pastPassPart vps.verb).short ! gn
      ++ vps.dep
      ++ np.s ! Ins
      ++ vps.compl1 ! Pos ! (genNumAgrP3 gn)
      ++ vps.compl2 ! Pos ! (genNumAgrP3 gn) ;
    isPost = False ;
    preferShort=PreferFull
    } ;

  -- : VPSlash -> VP ; -- be forced to sleep
  PassVPSlash vps = case vps.verb.asp of {
  Perfective => vps ** {
    verb=copulaEll ;
    compl=\\p,a => (pastPassPart vps.verb).short ! (agrGenNum a) ++ vps.compl1 ! p ! a ++ vps.compl2 ! p ! a ++ vps.c.s
    } ;
  Imperfective => vps ** {
    verb=(passivate vps.verb);
    compl=\\p,a => vps.compl1 ! p ! a ++ vps.compl2 ! p ! a ++ vps.c.s
  }
     };

  -- : VPSlash -> VP
  PassAgentVPSlash vps np = case vps.verb.asp of {
      Perfective => vps ** {
        verb=copulaEll ;
        compl=\\p,a => (pastPassPart vps.verb).short ! (agrGenNum a) ++ vps.c.s ++ vps.compl1 ! p ! a ++ vps.compl2 ! p ! a ++ np.s ! Ins
        } ;
      Imperfective => vps ** {
        verb=(passivate vps.verb);
        compl=\\p,a => vps.compl1 ! p ! a ++ vps.compl2 ! p ! a ++ np.s ! Ins
      }
     };

  -- Russian has no productive nominal gerund. The infinitive is the
  -- neutral fallback used by the multilingual Extend API.
  GerundCN vp = nounFormsNoun (immutableNounForms
    (vp.adv ! Ag (GSg Neut) P3 ++ verbInf vp.verb ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3)
    Neut Inanimate) ;

  GerundNP vp = {
    s=\\_ => vp.adv ! Ag (GSg Neut) P3 ++ verbInf vp.verb ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3 ;
    pron=False ;
    a=Ag (GSg Neut) P3
    } ;

  GerundAdv vp = {
    s=vp.adv ! Ag (GSg Neut) P3
      ++ vp.verb.prtr ++ verbRefl vp.verb ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3
    } ;

  ByVP vp = GerundAdv vp ;

  CompoundAP n a =
    let ap = adjFormsAdjective a in {
      s=\\gn,anim,cas => n.snom ++ "-" ++ ap.s ! gn ! anim ! cas ;
      short=\\agr => n.snom ++ "-" ++ ap.short ! agr ;
      isPost=False ;
      preferShort=a.preferShort
    } ;

  ProgrVPSlash vps = vps ;

  ReflRNP vps rnp = {
    verb=vps.verb ;
    adv=vps.adv ;
    dep=vps.dep ;
    compl=\\p,a => vps.compl1 ! p ! a ++ vps.c.s
      ++ rnp.s ! vps.c.c ++ vps.compl2 ! p ! a ;
    p=vps.p
    } ;

  ReflPron = sebya ;

  ReflPoss num cn = {
    s=\\cas =>
      (mkPronTable (reflexivePron (Ag (GSg Masc) P3)).poss)
        ! gennum cn.g (numSizeNumber num.size)
        ! cn.anim ! numSizeCase cas num.size
      ++ num.s ! cn.g ! cn.anim ! cas
      ++ cn.s ! animNumSizeNum cn.anim cas num.size
                ! numSizeCase cas num.size
    } ;

  PredetRNP pred rnp = {
    s=\\cas => pred.s ! GSg Masc ! Inanimate ! cas ++ rnp.s ! cas
    } ;

  AdvRNP np prep rnp = {
    s=\\cas => np.s ! cas ++ prep.s ++ rnp.s ! prep.c
    } ;

  AdvRVP vp prep rnp = vp ** {
    compl=\\p,a => vp.compl ! p ! a ++ prep.s ++ rnp.s ! prep.c
    } ;

  AdvRAP ap prep rnp = ap ** {
    s=\\gn,anim,cas => ap.s ! gn ! anim ! cas
      ++ prep.s ++ rnp.s ! prep.c ;
    short=\\a => ap.short ! a ++ prep.s ++ rnp.s ! prep.c ;
    isPost=True
    } ;

  ReflA2RNP a2 rnp = {
    s=\\gn,anim,cas =>
      (adjFormsAdjective a2).s ! gn ! anim ! cas
      ++ a2.c.s ++ rnp.s ! a2.c.c ;
    short=\\a =>
      (adjFormsAdjective a2).short ! a
      ++ a2.c.s ++ rnp.s ! a2.c.c ;
    isPost=False ;
    preferShort=a2.preferShort
    } ;

  PossPronRNP pron num cn rnp =
    let np = DetCN (DetQuant (PossPron pron) num) cn in {
      s=\\cas => np.s ! cas ++ rnp.s ! Gen ;
      pron=False ;
      a=np.a
      } ;

  ConjRNP conj xs = {
    s=\\cas => conj.s1 ++ xs.s1 ! cas ++ conj.s2 ++ xs.s2 ! cas
    } ;
  Base_rr_RNP x y = {s1=x.s ; s2=y.s} ;
  Base_nr_RNP x y = {s1=x.s ; s2=y.s} ;
  Base_rn_RNP x y = {s1=x.s ; s2=y.s} ;
  Cons_rr_RNP x xs = {
    s1=\\c => x.s ! c ++ comma ++ xs.s1 ! c ; s2=xs.s2
    } ;
  Cons_nr_RNP x xs = {
    s1=\\c => x.s ! c ++ comma ++ xs.s1 ! c ; s2=xs.s2
    } ;


  -- : Pron -> Pron ;  -- unstressed subject pronoun becomes empty: "am tired"
  ProDrop pron = {
    nom,gen,dat,acc,ins,prep=[] ;
    nPrefix=False ;
    poss={msnom,fsnom,nsnom,pnom,msgen,fsgen,pgen,msdat,fsacc,msins,fsins,pins,msprep=[]} ;
    a=pron.a
    } ;

  -- : AdV -> Utt ;                  -- always(!)
  UttAdV adv = {s=adv.s} ;

  -- : A -> AdV ;                    -- (that she) positively (sleeps)
  PositAdVAdj a = {s=a.short ! (GSg Neut); p=Pos} ;

  -- : NP -> SSlash -> Utt ; -- her I love
  FocusObj np ss = {
    s = applyPrep ss.c np ++ ss.s ! Ind
    } ;

  -- In Russian, sentence usually ends here (or special punctuation is needed after direct phrase)
  -- : VS -> Utt -> VP ;      -- say: "today"
  ComplDirectVS vs utt =
    AdvVP (UseV <lin V vs : V>) (lin Adv {s = ":" ++ rus_quoted utt.s}) ;
  -- : VQ -> Utt -> VP ;      -- ask: "when"
  ComplDirectVQ vq utt =
    AdvVP (UseV <lin V vq : V>) (lin Adv {s = ":" ++ rus_quoted utt.s}) ;

  -- : NP -> VS -> Utt -> Cl ;      -- "I am here", she said
  FrontComplDirectVS np vs utt = {
    subj = (rus_quoted utt.s) ++ "," ++ "—" ++ np.s ! Nom ;
    adv = [] ;
    verb = vs;
    dep = [] ;
    compl = \\_ => [] ;
    a = np.a
    } ;
  -- : NP -> VQ -> Utt -> Cl ;      -- "where", she asked
  FrontComplDirectVQ np vq utt = {
    subj = (rus_quoted utt.s) ++ "," ++ "—" ++ np.s ! Nom ;
    adv = [] ;
    verb = vq;
    dep = [] ;
    compl = \\_ => [] ;
    a = np.a
    } ;

  -- : Det -> NP ;
  DetNPFem det =
    let g = Fem in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  -- : Det -> NP ;
  DetNPMasc det =
    let g = Masc in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  UseDAP det =
    let g = Neut in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  -- : DAP -> NP ;
  UseDAPFem det =
    let g = Fem in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  -- : Det -> NP ;
  UseDAPMasc det =
    let g = Masc in {
      s=case det.type of {
        EmptyIndef => \\cas => a_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        EmptyDef => \\cas => the_Det.s ! g ! Inanimate ! cas ++ det.s ! g ! Inanimate ! cas ;
        _ => \\cas => det.s ! g ! Inanimate ! cas
        } ;
      pron=False ;
      a=Ag (gennum g (numSizeNumber det.size)) P3
      } ;

  BaseVPS = twoTable2 Mood Agr ;
  ConsVPS = consrTable2 Mood Agr comma ;
  ConjVPS = conjunctDistrTable2 Mood Agr ;

  BaseComp x y = {
    s1 = \\a => x.adv ++ x.s ! a ;
    s2 = \\a => y.adv ++ y.s ! a ;
    cop = x.cop
    } ;

  ConsComp x xs = {
    s1 = \\a => x.adv ++ x.s ! a ++ comma ++ xs.s1 ! a ;
    s2 = xs.s2 ;
    cop = x.cop
    } ;

  ConjComp conj xs = {
    s = \\a => conj.s1 ++ xs.s1 ! a ++ conj.s2 ++ xs.s2 ! a ;
    adv = [] ;
    cop = xs.cop
    } ;

  BaseImp x y = {s1=x.s ; s2=y.s} ;
  ConsImp x xs = {
    s1=\\p,gn => x.s ! p ! gn ++ comma ++ xs.s1 ! p ! gn ;
    s2=xs.s2
    } ;
  ConjImp conj xs = {
    s=\\p,gn => conj.s1 ++ xs.s1 ! p ! gn ++ conj.s2 ++ xs.s2 ! p ! gn
    } ;

  -- : NP -> VPS -> S ;
  PredVPS np vps = {
    s = \\m => np.s ! Nom ++ vps.s ! m ! np.a
    } ;

  -- : Temp -> Pol -> VP -> VPS ;
  MkVPS temp pol vp = {
    s = \\m,a =>
      let vpol = case orPol vp.p pol.p of {
            Neg => PNeg ;
            Pos => PPos
            }
      in temp.s
         ++ verbEnvAgr [] (vp.adv ! a) vp.verb m temp.t a vpol
         ++ vp.dep
         ++ vp.compl ! vpol.p ! a
    } ;

oper
  rus_quoted : Str -> Str = \s -> "«" ++ s ++ "»" ; ---- TODO bind ; move to Prelude?

} ;
