--# -path=../common:../abstract

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
    -- BaseVPS, ConsVPS, BaseVPI, ConsVPI, BaseVPS2, ConsVPS2, BaseVPI2, ConsVPI2,
    -- MkVPS, ConjVPS, MkVPI, ConjVPI, ComplVPIVV,
    -- MkVPS2, ConjVPS2, ComplVPS2, MkVPI2, ConjVPI2, ComplVPI2,
    -- Base_nr_RNP, Base_rn_RNP, Base_rr_RNP, ByVP, CompBareCN,
    -- CompQS, CompS, CompVP, ComplBareVS, ComplGenVV, ComplSlashPartLast, ComplVPSVV, CompoundAP,
    CompoundN,

    --ConjRNP, Cons_nr_RNP, Cons_rr_RNP,
    DetNPMasc,
    DetNPFem,
    -- EmbedPresPart, EmptyRelSlash,
    ExistsNP,
    -- ExistCN, ExistMassCN, ExistPluralCN,
    --ProDrop,
    -- FocusAP, FocusAdV, FocusAdv,
    FocusObj,
    -- GenIP, GenModIP, GenModNP, GenNP, GenRP,
    -- GerundAdv, GerundCN, GerundNP, IAdvAdv, ICompAP,
    InOrderToVP,
    -- NominalizeVPSlashNP,
    -- PassAgentVPSlash,
    PassVPSlash,
    -- ProgrVPSlash,
    PastPartAP,
    PastPartAgentAP,
    PositAdVAdj,
    -- PredVPS, PredVPSVV, PredetRNP, PrepCN,
    -- EmbedSSlash, PresPartAP,
    PurposeVP,
    -- ReflPoss, ReflPron, ReflRNP, SlashBareV2S, SlashV2V, StrandQuestSlash, StrandRelSlash,
    PredIAdvVP,
    -- UncontractedNeg, UttAccIP, UttAccNP,
    FrontComplDirectVS,
    FrontComplDirectVQ,
    UttAdV
    -- UttDatIP, UttDatNP, UttVPShort, WithoutVP
   ]
  with (Grammar=GrammarRus)
  ** open Prelude, ResRus, ParadigmsRus, (M = MorphoRus) in {

lincat
  RNP     = {s : Agr => Str} ;
  RNPList = {s1,s2 : Agr => Str} ;

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
  InOrderToVP vp = lin Adv ({
    s = "чтобы"
      ++ vp.adv ! Ag (GSg Neut) P3
      ++ (verbInf vp.verb)
      ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3
    }) ;

  -- : VP -> Adv ;  -- to become happy
  PurposeVP vp = lin Adv ({
    s = vp.adv ! Ag (GSg Neut) P3 ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! Ag (GSg Neut) P3
    }) ;

  -- : NP -> Cl ;  -- there exists a number / there exist numbers
  ExistsNP np = {
    subj=[] ;
    adv=[] ;
    verb=M.to_exist ;
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
  CompoundN n1 n2 = mkCompoundN n1 "-" n2 ;

  -- VPSlash -> AP ; -- lost (opportunity) ; (opportunity) lost in space
  PastPartAP vps = {
    s=\\gn,anim,cas =>
      vps.adv ! (genNumAgrP3 gn)
      ++ shortPastPassPart vps.verb gn
      ++ vps.dep
      ++ vps.compl ! Pos ! (genNumAgrP3 gn) ;
    short=\\a =>
      vps.adv ! a
      ++ shortPastPassPart vps.verb (agrGenNum a)
      ++ vps.dep
      ++ vps.compl ! Pos ! a
      ++ vps.c.s ; --
    isPost = False ;
    preferShort=PreferFull
    } ;

  -- : VPSlash -> NP -> AP ;   -- (opportunity) lost by the company
  PastPartAgentAP vps np ={
    s=\\gn,anim,cas =>
      vps.adv ! (genNumAgrP3 gn)
      ++ shortPastPassPart vps.verb gn
      ++ vps.dep
      ++ applyPolPrep Pos vps.c np
      ++ vps.compl ! Pos ! (genNumAgrP3 gn) ;
    short=\\a =>
      vps.adv ! a
      ++ shortPastPassPart vps.verb (agrGenNum a)
      ++ vps.dep
      ++ applyPolPrep Pos vps.c np
      ++ vps.compl ! Pos ! a ;
    isPost = False ;
    preferShort=PreferFull
    } ;

  -- : VPSlash -> VP ; -- be forced to sleep
  PassVPSlash vps = vps ** {
    verb=copulaEll ;
    compl=\\p,a => vps.compl ! p ! a ++ shortPastPassPart vps.verb (agrGenNum a) ++ vps.c.s
    } ;
  -- PresPartAP    : VP -> AP ;   -- (the man) looking at Mary
  -- use PlP2 + "ый"

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
  PositAdVAdj a = ss a.sn ;

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

oper
  rus_quoted : Str -> Str = \s -> "«" ++ s ++ "»" ; ---- TODO bind ; move to Prelude?
} ;
