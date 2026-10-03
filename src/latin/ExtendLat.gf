--1 Extensions of core RGL syntax (the Grammar module)

-- This module defines syntax rules that are not yet implemented in all
-- languages, and perhaps never implementable either. But all rules are given
-- a default implementation in common/ExtendFunctor.gf so that they can be included
-- in the library API. The default implementations are meant to be overridden in each
-- xxxxx/ExtendXxx.gf when the work proceeds.
--
-- This module is aimed to replace the original Extra.gf, which is kept alive just
-- for backwardcommon compatibility. It will also replace translator/Extensions.gf
-- and thus eliminate the often duplicated work in those two modules.
--
-- (c) Aarne Ranta 2017-08-20 under LGPL and BSD


concrete ExtendLat of Extend = CatLat ** open ResLat, Prelude in {

  lincat
    VPS = {s : Gender => Number => Person => Str} ;
    [VPS] = {s : Coordinator => {init,last : Gender => Number => Person => Str}} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s : Coordinator => {init,last : Agr => Str}} ;
    [Imp] = {s : Coordinator => {init,last : Polarity => VImpForm => Str}} ;
    [Comp] = {s : Coordinator => {init,last : Agr => Str}} ;
    RNP = {s : Agr => Case => Str} ;
    RNPList = {s : Agr => Case => Str} ;

  oper
    reflPron : Case => Str = table {
      Nom | Voc => ""; Acc | Abl => "se"; Gen => "sui"; Dat => "sibi"
      } ;

  lin
    -- GenNP       : NP -> Quant ;       -- this man's
    GenNP np = { s = \\_ => combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Gen ; sp = \\_ => ""} ;

    GenModNP num np cn = {
      s = \\_,c => cn.s ! num.n ! c ++ combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Gen ;
      n = num.n; g = cn.g; p = P3; adv = cn.adv;
      preap = cn.preap; postap = cn.postap;
      det = {s,sp = \\_ => ""; n = num.n}
      } ;

    UseDAP dap = {
      s = \\_,c => dap.s ! Neutr ! c; n=dap.n; g=Neutr; p=P3; adv="";
      preap,postap={s=\\_=>""}; det={s,sp=\\_=>"";n=dap.n}
      } ;
    UseDAPMasc dap = {
      s = \\_,c => dap.s ! Masc ! c; n=dap.n; g=Masc; p=P3; adv="";
      preap,postap={s=\\_=>""}; det={s,sp=\\_=>"";n=dap.n}
      } ;
    UseDAPFem dap = {
      s = \\_,c => dap.s ! Fem ! c; n=dap.n; g=Fem; p=P3; adv="";
      preap,postap={s=\\_=>""}; det={s,sp=\\_=>"";n=dap.n}
      } ;

    EmptyRelSlash slash = {s = \\_,_ => slash} ;

    MkVPS t pol vp = {s = \\g,n,p =>
      vp.adv ++ vp.obj ++ pol.s ++ vp.compl ! Ag g n Nom ++ t.s ++
      vp.s ! VAct (anteriorityToVAnter t.a) (tenseToVTense t.t) n p ! VQFalse} ;
    BaseVPS x y = {s = \\_ => {init=x.s; last=y.s}} ;
    ConsVPS x xs = {s = \\c => {
      init = \\g,n,p => (xs.s ! c).init ! g ! n ! p ++ bindComma ++ (xs.s ! c).last ! g ! n ! p;
      last = x.s}} ;
    ConjVPS conj xs = {s = \\g,n,p => conj.s1 ++
      (xs.s ! conj.c).init ! g ! n ! p ++ conj.s2 ++
      (xs.s ! conj.c).last ! g ! n ! p ++ conj.s3} ;
    PredVPS np vps = (combineClause "" (mkClause np emptyVP) Pres Simul Pos VQFalse) ** {
      v = \\_ => vps.s ! np.g ! np.n ! np.p
      } ;

    MkVPI vp = {s = \\a => vp.adv ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres} ;
    BaseVPI x y = {s = \\_ => {init=x.s; last=y.s}} ;
    ConsVPI x xs = {s = \\c => {
      init = \\a => (xs.s ! c).init ! a ++ bindComma ++ (xs.s ! c).last ! a;
      last = x.s}} ;
    ConjVPI conj xs = {s = \\a => conj.s1 ++
      (xs.s ! conj.c).init ! a ++ conj.s2 ++
      (xs.s ! conj.c).last ! a ++ conj.s3} ;
    ComplVPIVV vv vpi = (predV vv) ** {compl = vpi.s} ;
    BaseImp x y = {s=\\_=>{init=x.s;last=y.s}} ;
    ConsImp x xs = {s=\\c=>{init=\\p,f=>(xs.s!c).init!p!f++bindComma++(xs.s!c).last!p!f;last=x.s}} ;
    ConjImp conj xs = {s=\\p,f=>conj.s1++(xs.s!conj.c).init!p!f++conj.s2++(xs.s!conj.c).last!p!f++conj.s3} ;
    BaseComp x y = {s = \\_ => {init = x.s; last = y.s}} ;
    ConsComp x xs = {s = \\c => {
      init = \\a => (xs.s ! c).init ! a ++ bindComma ++ (xs.s ! c).last ! a;
      last = x.s}} ;
    ConjComp conj xs = {s = \\a => conj.s1 ++
      (xs.s ! conj.c).init ! a ++ conj.s2 ++
      (xs.s ! conj.c).last ! a ++ conj.s3} ;
--     GenIP       : IP -> IQuant ;      -- whose
--     GenRP       : Num -> CN -> RP ;   -- whose car

-- -- In case the first two are not available, the following applications should in any case be.

--     GenModNP    : Num -> NP -> CN -> NP ; -- this man's car(s)
--     GenModIP    : Num -> IP -> CN -> IP ; -- whose car(s)

--     CompBareCN  : CN -> Comp ;        -- (is) teacher

--     StrandQuestSlash : IP -> ClSlash -> QCl ;   -- whom does John live with
--     StrandRelSlash   : RP -> ClSlash -> RCl ;   -- that he lives in
--     EmptyRelSlash    : ClSlash       -> RCl ;   -- he lives in


-- -- $VP$ conjunction, separate categories for finite and infinitive forms (VPS and VPI, respectively)
-- -- covering both in the same category leads to spurious VPI parses because VPS depends on many more tenses

--   cat
--     VPS ;           -- finite VP's with tense and polarity
--     [VPS] {2} ;
--     VPI ;
--     [VPI] {2} ;     -- infinitive VP's (TODO: with anteriority and polarity)

--   fun
--     MkVPS      : Temp -> Pol -> VP -> VPS ;  -- hasn't slept
--     ConjVPS    : Conj -> [VPS] -> VPS ;      -- has walked and won't sleep
--     PredVPS    : NP   -> VPS -> S ;          -- she [has walked and won't sleep]

--     MkVPI      : VP -> VPI ;                 -- to sleep (TODO: Ant and Pol)
--     ConjVPI    : Conj -> [VPI] -> VPI ;      -- to sleep and to walk
--     ComplVPIVV : VV   -> VPI -> VP ;         -- must sleep and walk

-- -- the same for VPSlash, taking a complement with shared V2 verbs

--   cat
--     VPS2 ;        -- have loved (binary version of VPS)
--     [VPS2] {2} ;  -- has loved, hates"
--     VPI2 ;        -- to love (binary version of VPI)
--     [VPI2] {2} ;  -- to love, to hate

--   fun
--     MkVPS2    : Temp -> Pol -> VPSlash -> VPS2 ;  -- has loved
--     ConjVPS2  : Conj -> [VPS2] -> VPS2 ;          -- has loved and now hates
--     ComplVPS2 : VPS2 -> NP -> VPS ;               -- has loved and now hates that person

--     MkVPI2    : VPSlash -> VPI2 ;                 -- to love
--     ConjVPI2  : Conj -> [VPI2] -> VPI2 ;          -- to love and hate
--     ComplVPI2 : VPI2 -> NP -> VPI ;               -- to love and hate that person

--   fun
--     ProDrop : Pron -> Pron ;  -- unstressed subject pronoun becomes empty: "am tired"

--     ICompAP : AP -> IComp ;   -- "how old"
--     IAdvAdv : Adv -> IAdv ;   -- "how often"

--     CompIQuant : IQuant -> IComp ;   -- which (is it) [agreement to NP]

--     PrepCN     : Prep -> CN -> Adv ; -- by accident [Prep + CN without article]

--   -- fronted/focal constructions, only for main clauses

--   fun
--     FocusObj : NP  -> SSlash  -> Utt ;   -- her I love
--     FocusAdv : Adv -> S       -> Utt ;   -- today I will sleep
--     FocusAdV : AdV -> S       -> Utt ;   -- never will I sleep
--     FocusAP  : AP  -> NP      -> Utt ;   -- green was the tree

--   -- participle constructions
--     PresPartAP    : VP -> AP ;   -- (the man) looking at Mary
--     EmbedPresPart : VP -> SC ;   -- looking at Mary (is fun)

    --     PastPartAP      : VPSlash -> AP ;         -- lost (opportunity) ; (opportunity) lost in space
    PastPartAP vp = { s = \\ag => vp.part ! VPassPerf ! ag ++ vp.adv ++ vp.c.s} ; -- TODO
    PastPartAgentAP vp np = {s = \\ag => vp.part ! VPassPerf ! ag ++ vp.adv ++
      "ab" ++ combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Abl} ;
    PresPartAP vp = {s = \\ag => vp.part ! VActPres ! ag ++ vp.obj ++ vp.compl ! ag ++ vp.adv} ;
--     PastPartAgentAP : VPSlash -> NP -> AP ;   -- (opportunity) lost by the company

-- -- this is a generalization of Verb.PassV2 and should replace it in the future.

    --     PassVPSlash : VPSlash -> VP ; -- be forced to sleep
    PassVPSlash vp = vp ** {
      s = \\a => case a of { VAct _ t n p => vp.pass ! VPass t n p } ;
      } ;
    PassAgentVPSlash vp np = (PassVPSlash vp) ** {
      adv = vp.adv ++ "ab" ++ combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Abl
      } ;
    ComplBareVS vs s = vs ** {
      s = \\a,q => vs.act ! a; pass=\\p,q=>vs.pass ! p;
      compl=\\_=>defaultSentence s ! SOV; adv=""; obj=""
      } ;
    ProgrVPSlash vp = vp ;

-- -- the form with an agent may result in a different linearization
-- -- from an adverbial modification by an agent phrase.

--     PassAgentVPSlash : VPSlash -> NP -> VP ;  -- be begged by her to go

-- -- publishing of the document

--     NominalizeVPSlashNP : VPSlash -> NP -> NP ;

-- -- counterpart to ProgrVP, for VPSlash

--     ProgrVPSlash : VPSlash -> VPSlash;

-- -- existential for mathematics

--     ExistsNP : NP -> Cl ;  -- there exists a number / there exist numbers

-- -- existentials with a/no variation

--     ExistCN       : CN -> Cl ;  -- there is a car / there is no car
--     ExistMassCN   : CN -> Cl ;  -- there is beer / there is no beer
--     ExistPluralCN : CN -> Cl ;  -- there are trees / there are no trees

-- -- generalisation of existential, with adverb as a parameter
--     AdvIsNP : Adv -> NP -> Cl ;  -- here is the tree / here are the trees
--     AdvIsNPAP : Adv -> NP -> AP -> Cl ; -- here are the instructions documented

-- -- infinitive for purpose AR 21/8/2013

--     PurposeVP : VP -> Adv ;  -- to become happy

-- -- object S without "that"

--     ComplBareVS  : VS  -> S  -> VP ;       -- say she runs
--     SlashBareV2S : V2S -> S  -> VPSlash ;  -- answer (to him) it is good

--     ComplDirectVS : VS -> Utt -> VP ;      -- say: "today"
--     ComplDirectVQ : VQ -> Utt -> VP ;      -- ask: "when"

-- -- front the extraposed part

--     FrontComplDirectVS : NP -> VS -> Utt -> Cl ;      -- "I am here", she said
--     FrontComplDirectVQ : NP -> VQ -> Utt -> Cl ;      -- "where", she asked

-- -- proper structure of "it is AP to VP"

--     PredAPVP : AP -> VP -> Cl ;      -- it is good to walk

-- -- to use an AP as CN or NP without CN

    --     AdjAsCN : AP -> CN ;   -- a green one ; en grön (Swe)
    --     AdjAsNP : AP -> NP ;   -- green (is good)
    AdjAsNP ap = {
      s = \\_,c => ap.s ! (Ag Neutr Sg c) ;
      adv = "" ;
      det = { s, sp = \\_ => "" } ;
      g = Neutr ;
      n = Sg ;
      p = P3 ;
      postap = { s = \\_ => "" } ;
      preap = { s = \\_ => "" } ;
      } ;

    ExistsNP np = mkClause np (predV esseAux) ;
    AdvIsNP adv np = mkClause np (insertAdv adv (predV esseAux)) ;
    ExistMassCN cn = mkClause (cn ** {s=\\_,c=>cn.s ! Sg ! c; n=Sg;p=P3;
      det={s,sp=\\_=>"";n=Sg}}) (predV esseAux) ;
    ExistPluralCN cn = mkClause (cn ** {s=\\_,c=>cn.s ! Pl ! c; n=Pl;p=P3;
      det={s,sp=\\_=>"";n=Pl}}) (predV esseAux) ;
    PrepCN prep cn = mkAdverb (prep.s ++ cn.s ! Sg ! prep.c) ;
    CompBareCN cn = {s = \\a => case a of {Ag _ n c => cn.s ! n ! c}} ;
    AdjAsCN ap = {s=\\n,c=>ap.s ! Ag Neutr n c;g=Neutr;
      preap,postap={s=\\_=>""};adv=""} ;

    ReflPron = {s = \\_,c => reflPron ! c} ;
    ReflPoss num cn = {s = \\_,c =>
      (createPronouns Masc Sg P3).p2 ! PronRefl ! Ag cn.g num.n c ++ cn.s ! num.n ! c} ;
    ReflRNP vp rnp = vp ** {
      compl = \\a => rnp.s ! a ! vp.c.c ++ vp.compl ! a
      } ;
    AdvRNP np prep rnp = {s = \\a,c =>
      combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! c ++ prep.s ++ rnp.s ! a ! prep.c} ;
    AdvRVP vp prep rnp = vp ** {compl = \\a => vp.compl ! a ++ prep.s ++ rnp.s ! a ! prep.c} ;
    AdvRAP ap prep rnp = {s = \\a => ap.s ! a ++ prep.s ++ rnp.s ! a ! prep.c} ;
    ReflA2RNP a rnp = {s = \\ag => a.s ! Posit ! ag ++ a.c.s ++ rnp.s ! ag ! a.c.c} ;
    PossPronRNP pron num cn rnp =
      let base = {
        s = \\_,c => cn.s ! num.n ! c;
        n = num.n; g = cn.g; p = P3; adv = rnp.s ! Ag pron.pers.g pron.pers.n Nom ! Gen;
        preap=cn.preap; postap=cn.postap;
        det={s=\\c=>pron.poss.s ! PronNonRefl ! Ag cn.g num.n c; sp=\\_=>""; n=num.n}
        }
      in base ;

    CompoundN n1 n2 = {s = \\n,c => n2.s ! n ! c ++ n1.s ! Sg ! Gen; g=n2.g} ;
    CompoundAP n a = {s = \\ag => n.s ! Sg ! Gen ++ a.s ! Posit ! ag} ;
    GerundCN vp = {s = \\_,_ => vp.inf ! VInfActPres ++ vp.obj ++ vp.adv;
      g=Neutr; preap,postap={s=\\_=>""}; adv=""} ;
    GerundNP vp = dummyNP (vp.inf ! VInfActPres ++ vp.obj ++ vp.adv) ;
    GerundAdv vp = mkAdverb (vp.inf ! VInfActPres ++ vp.obj ++ vp.adv) ;
    ByVP vp = mkAdverb ("gerundio" ++ vp.inf ! VInfActPres ++ vp.obj ++ vp.adv) ;
    InOrderToVP vp = mkAdverb ("ut" ++ vp.inf ! VInfActPres ++ vp.obj ++ vp.adv) ;
    ApposNP np app = np ** {adv = np.adv ++ bindComma ++
      combineNounPhrase app ! PronNonDrop ! APostN ! DPreN ! Nom ++ bindComma} ;
    PositAdVAdj a = {s = a.adv.s ! Posit} ;
    CompS s = {s = \\_ => "quod" ++ defaultSentence s ! SOV} ;
    CompQS qs = {s = \\_ => qs.s ! QIndir} ;
    CompVP ant pol vp = {s = \\a => pol.s ++ vp.inf ! VInfActPres ++ vp.obj ++ vp.compl ! a ++ vp.adv} ;
    ComplSlashPartLast vp np = insertObj np vp.c vp ;
    UttVPShort vp = {s = vp.imp ! VImp1 Sg ++ vp.obj ++ vp.compl ! Ag Masc Sg Acc ++ vp.adv} ;

-- -- infinitive complement for IAdv

--     PredIAdvVP : IAdv -> VP -> QCl ; -- how to walk?

-- -- alternative to EmbedQS. For English, EmbedQS happens to work,
-- -- because "what" introduces question and relative. The default linearization
-- -- could be e.g. "the thing we did (was fun)".

--     EmbedSSlash : SSlash -> SC  ;   -- what we did (was fun)

-- -- reflexive noun phrases: a generalization of Verb.ReflVP, which covers just reflexive pronouns
-- -- This is necessary in languages like Swedish, which have special reflexive possessives.
-- -- However, it is also needed in application grammars that want to treat "brush one's teeth" as a one-place predicate.

--   cat
--     RNP ;     -- reflexive noun phrase, e.g. "my family and myself"
--     RNPList ; -- list of reflexives to be coordinated, e.g. "my family, myself, everyone"

-- -- Notice that it is enough for one NP in RNPList to be RNP.

--   fun
--     ReflRNP : VPSlash -> RNP -> VP ;   -- love my family and myself

--     ReflPron : RNP ;                   -- myself
--     ReflPoss : Num -> CN -> RNP ;      -- my car(s)

--     PredetRNP : Predet -> RNP -> RNP ; -- all my brothers

--     ConjRNP : Conj -> RNPList -> RNP ;  -- my family, John and myself

--     Base_rr_RNP : RNP -> RNP -> RNPList ;       -- my family, myself
--     Base_nr_RNP : NP  -> RNP -> RNPList ;       -- John, myself
--     Base_rn_RNP : RNP -> NP  -> RNPList ;       -- myself, John
--     Cons_rr_RNP : RNP -> RNPList -> RNPList ;   -- my family, myself, John
--     Cons_nr_RNP : NP  -> RNPList -> RNPList ;   -- John, my family, myself
-- ----    Cons_rn_RNP : RNP -> ListNP  -> RNPList ;   -- myself, John, Mary


-- --- from Extensions

--   ComplGenVV  : VV -> Ant -> Pol -> VP  -> VP ;         -- want not to have slept
-- ----  SlashV2V    : V2V -> Ant -> Pol -> VPS -> VPSlash ;   -- force (her) not to have slept

--   CompoundN   : N -> N  -> N ;      -- control system / controls system / control-system
--   CompoundAP  : N -> A  -> AP ;     -- language independent / language-independent

--   GerundCN    : VP -> CN ;          -- publishing of the document (can get a determiner)
--   GerundNP    : VP -> NP ;          -- publishing the document (by nature definite)
--   GerundAdv   : VP -> Adv ;         -- publishing the document (prepositionless adverb)

--   WithoutVP   : VP -> Adv ;         -- without publishing the document
--   ByVP        : VP -> Adv ;         -- by publishing the document
--   InOrderToVP : VP -> Adv ;         -- (in order) to publish the document

--   ApposNP : NP -> NP -> NP ;        -- Mr Macron, the president of France,

--   AdAdV       : AdA -> AdV -> AdV ;           -- almost always
    UttAdV adv = {s = adv.s} ;
--   PositAdVAdj : A -> AdV ;                    -- (that she) positively (sleeps)

--   CompS       : S -> Comp ;                   -- (the fact is) that she sleeps
--   CompQS      : QS -> Comp ;                  -- (the question is) who sleeps
--   CompVP      : Ant -> Pol -> VP -> Comp ;    -- (she is) to go

-- -- very language-specific things

-- -- Eng
--   UncontractedNeg : Pol ;      -- do not, etc, as opposed to don't
--   UttVPShort : VP -> Utt ;     -- have fun, as opposed to "to have fun"
--   ComplSlashPartLast : VPSlash -> NP -> VP ; -- set it apart, as opposed to "set apart it"

-- -- Romance
--   DetNPMasc : Det -> NP ;
--   DetNPFem  : Det -> NP ;

--   UseComp_estar : Comp -> VP ; -- (Cat, Spa, Por) "está cheio" instead of "é cheio"

--   SubjRelNP : NP -> RS -> NP ; -- Force RS in subjunctive: lo que les *resulte* mejor

--   iFem_Pron      : Pron ; -- I (Fem)
--   youFem_Pron    : Pron ; -- you (Fem)
--   weFem_Pron     : Pron ; -- we (Fem)
--   youPlFem_Pron  : Pron ; -- you plural (Fem)
--   theyFem_Pron   : Pron ; -- they (Fem)
--   youPolFem_Pron : Pron ; -- you polite (Fem)
--   youPolPl_Pron  : Pron ; -- you polite plural (Masc)
--   youPolPlFem_Pron : Pron ; -- you polite plural (Fem)

-- -- German
--   UttAccNP : NP -> Utt ; -- him (accusative)
--   UttDatNP : NP -> Utt ; -- him (dative)
--   UttAccIP : IP -> Utt ; -- whom (accusative)
--   UttDatIP : IP -> Utt ; -- whom (dative)


   TPastSimple = {s = []} ** {t = Past} ;   --# notpresent

}
