concrete ExtendCze of Extend = CatCze ** 
  ExtendFunctor - [
    RNP, RNPList, ReflRNP, ReflPron, ReflPoss, PredetRNP,
    ConjRNP, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP,
    ReflPossPron, ProDrop,
    iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron,
    theyFem_Pron, theyNeutr_Pron, youPolFem_Pron, youPolPlFem_Pron
    ---- constant not found (yet)
    ,UttVPShort
    ,UttAccIP
    ,UttDatIP
    ,SubjRelNP
    ,StrandRelSlash
    ,StrandQuestSlash
    ,SlashBareV2S
    ,PredIAdvVP
    ,PredAPVP
    ,ExistS
    ,ExistPluralCN
    ,ExistNPQS
    ,ExistMassCN
    ,ExistIPQS
    ,ExistCN
    ,EmptyRelSlash
    ,DetNPMasc
    ,DetNPFem
    ,ComplBareVS
    ,CompIQuant
    ,CompBareCN
    ,PiedPipingQuestSlash
    ,PiedPipingRelSlash
    ,TPastSimple
    ]
  with (Grammar = GrammarCze)
    **
open
  ResCze, Prelude, (S = SyntaxCze), (P = ParadigmsCze)
in {

lincat
  RNP = BoundNPForms ** {m : RNPHead ; isPron : Bool} ;
  RNPList = {s1,s2,prep1,prep2 : Agr => Case => Str ; m : RNPHead} ;

param
  RNPHead = AntecedentHead | FixedHead ModifierAgr ;

lin
  -- Retain full forms for objects, coordination and NP modifiers.
  ProDrop pron = pron ** {isDrop = True} ;

  iFem_Pron = P.genderPron Fem S.i_Pron ;
  youFem_Pron = P.genderPron Fem S.youSg_Pron ;
  weFem_Pron = P.genderPron Fem S.we_Pron ;
  youPlFem_Pron = P.genderPron Fem S.youPl_Pron ;
  theyFem_Pron = P.genderPron Fem S.they_Pron ;
  theyNeutr_Pron = P.genderPron Neutr S.they_Pron ;
  youPolFem_Pron = P.genderPron Fem S.youPol_Pron ;
  youPolPlFem_Pron = P.genderPron Fem S.youPl_Pron ;

  ReflPossPron = justDemPronFormsAdjective reflPossessivePron ;

  -- RNPs are full noun phrases. ReflPron consequently uses sebe/sobě,
  -- including in coordination, rather than a lexical reflexive clitic.
  ReflPron =
    let s : Case => Str = table {
      Nom | ResCze.Voc => nonExist ; Gen | Acc => "sebe" ;
      Dat | Loc => "sobě" ; Ins => "sebou"
      }
    in boundNPForms (\\_ => npForms s s) ** {
      m = AntecedentHead ; isPron = True
      } ;
  ReflPoss num cn = fullRNP (S.mkNP (lin Quant (justDemPronFormsAdjective reflPossessivePron))
    <lin Num num : S.Num> <lin CN cn : S.CN>) ;
  ReflRNP vps rnp = vps ** {
    clit = \\a => vps.clit ! a ++ vps.clitAfter ! a ;
    compl = \\a => vps.compl ! a ++
      fullComplement vps.c (rnp.s ! a) (rnp.prep ! a) ++ vps.ind ! a
    } ;
  PredetRNP pred rnp = rnp ** boundNPForms (\\a =>
    predetNPForms (andB pred.postPron rnp.isPron)
      (\\c => predetForm pred (rnpAgr rnp.m a) c) (rnpForms rnp a)) ;
  AdvRNP np p rnp = boundNPForms (\\a =>
    appendNPForms np (fullComplement p (rnp.s ! a) (rnp.prep ! a))) ** {
      m = FixedHead np.m ; isPron = np.isPron
      } ;
  AdvRVP vp p rnp = vp ** {
    compl = \\a => vp.compl ! a ++ fullComplement p (rnp.s ! a) (rnp.prep ! a)
    } ;
  AdvRAP ap p rnp = ap ** {
    s = \\g,n,c => ap.s ! g ! n ! c ++ fullComplement p (rnp.s ! Ag g n P3) (rnp.prep ! Ag g n P3) ;
    pred = \\a => ap.pred ! a ++ fullComplement p (rnp.s ! a) (rnp.prep ! a) ; isPost = True
    } ;
  Base_rr_RNP x y = baseRNP (lin RNP x) (lin RNP y) ;
  Base_nr_RNP x y = baseRNP (fullRNP (lin NP x)) (lin RNP y) ;
  Base_rn_RNP x y = baseRNP (lin RNP x) (fullRNP (lin NP y)) ;
  Cons_rr_RNP x xs = consRNP (lin RNP x) (lin RNPList xs) ;
  Cons_nr_RNP x xs = consRNP (fullRNP (lin NP x)) (lin RNPList xs) ;
  ConjRNP conj xs = boundNPForms (\\a => npForms
    (\\c => conj.s1 ++ xs.s1 ! a ! c ++ conj.s2 ++ xs.s2 ! a ! c)
    (\\c => conj.s1 ++ xs.prep1 ! a ! c ++ conj.s2 ++ xs.prep2 ! a ! c)) ** {
      m = xs.m ; isPron = False
      } ;

oper
  BoundNPForms : Type = {
    s,prep,before,prepBefore : Agr => Case => Str ; after : Agr => Str
    } ;
  -- Placement uses the same NP operations at each antecedent agreement.
  boundNPForms : (Agr => NPForms) -> BoundNPForms = \forms -> {
    s = \\a => (forms ! a).s ; prep = \\a => (forms ! a).prep ;
    before = \\a => (forms ! a).before ; prepBefore = \\a => (forms ! a).prepBefore ;
    after = \\a => (forms ! a).after
    } ;
  rnpForms : RNP -> Agr -> NPForms = \rnp,a -> {
    s = rnp.s ! a ; prep = rnp.prep ! a ;
    before = rnp.before ! a ; prepBefore = rnp.prepBefore ! a ;
    after = rnp.after ! a
    } ;
  -- Ordinary NPs and possessed heads have their own modifier agreement.
  fullRNP : S.NP -> RNP = \np -> lin RNP (boundNPForms (\\_ => np) ** {
    m = FixedHead np.m ; isPron = np.isPron
    }) ;
  -- A reflexive inherits gender and number, but its own complement position
  -- selects case: pět dětí miluje sebe všechny, not sebe všech.
  rnpAgr : RNPHead -> Agr -> ModifierAgr = \head,a -> case head of {
    AntecedentHead => case a of {
      AgQuant g => Mod g Pl ; _ => modifierAgr a
      } ;
    FixedHead m => m
    } ;
  -- As for ordinary NPs, preposed modifiers agree with the first conjunct.
  baseRNP : RNP -> RNP -> RNPList = \x,y -> lin RNPList {
    s1 = x.s ; s2 = y.s ; prep1 = x.prep ; prep2 = y.prep ; m = x.m
    } ;
  consRNP : RNP -> RNPList -> RNPList = \x,xs -> xs ** {
    s1 = \\a,c => x.s ! a ! c ++ SOFT_BIND ++ "," ++ xs.s1 ! a ! c ;
    prep1 = \\a,c => x.prep ! a ! c ++ SOFT_BIND ++ "," ++ xs.prep1 ! a ! c ;
    m = x.m
    } ;


}
