--# -path=.:../abstract:../common:../../prelude
--
--1 Persian auxiliary operations.
--
-- This module contains operations that are needed to make the
-- resource syntax work.

resource ResPes = MorphoPes ** open Prelude,Predef in {

  flags optimize=all ;
  coding = utf8;

  param
    Order = ODir | OQuest ;
    CardOrd = NCard | NOrd ;
    RAgr = RNoAg | RAg Agr ;
    RelPron = Ance | Ke ; -- https://en.wiktionary.org/wiki/%D8%A2%D9%86%DA%86%D9%87
    WordOrder = OV | VO ; -- for showVPH

  oper
    CN : Type = Noun ** {
      hasAdj : Bool ;    -- to get the right form when CN is a predicate
      compl : Number => Str -- to make possessive suffix attach to the right word
                            -- dep. on Number because of RelCN
      } ;

    NP : Type = BaseNP ** {
      s : Mod => Str ; -- NP can appear with a clitic, need to keep Mod open
      } ;

    BaseNP : Type = {
      a : Agr ;
      isNeg : Bool ;  -- negative determiner forces negation in Cl and S
      takesYeAsComp : Bool ; -- to get the right form when NP is a predicate
      animacy : Animacy ; -- to get the right pronoun in FunRP
      isClitic : Bool ; -- if isPron, becomes clitic as a direct object
      clitic : Str ;
      relpron : RelPron ; -- contraction for "that which"
      empty : Str -- to prevent metavariables in case of rel.pron. contraction
      } ;

  oper
    emptyNP : NP = {
      s = \\_ => [] ;
      a = defaultAgr ;
      isNeg = False ;
      takesYeAsComp = False ;
      animacy = Inanimate ;
      isClitic = False ;
      clitic = [] ;
      relpron = Ke ;
      empty = []
      } ;

    indeclNP : Str -> NP = \s ->
      emptyNP ** {s = \\m => s} ;

    useN : Noun -> CN = \n -> n ** {
      hasAdj = False ;
      compl = \\_ => []
    } ;

    np2str : NP -> Str = \np ->
      np.s ! Bare ;

    cn2str : CN -> Str = \cn ->
      cn.s ! Sg ! Bare ++ cn.compl ! Sg ;

    rs2str : RelPron -> Agr -> {s : Agr => Str ; rp : RelPron => Str} -> Str =
      \ke,agr,rs -> rs.rp ! ke ++ rs.s ! agr ;

 -----------------------
 --- Verb Phrase
 -----------------------
param
  VVType = NoVV | FullVV | DefVV ;
  VVForm = Indic | Subj ; ---| SubjPast ; -- TODO extend this to VV, VS and Subj
  VVTense = VVPres | VVPast VVForm ;
  TAnt = Ind Tense Anteriority | Sub Anteriority ;

oper

  -- VVPast Subj is another possibility, used in constructions such as
  -- قاتل نمی توانسته آنجا بوده باشد، چون او آن زمان در پاریس بوده
  -- The form is created in complVV, but not currently used in other functions. /IL
  ta2vvt : TAnt -> VVType -> VVTense = \ta,vvtype -> case ta of {
    Ind Pres Anter |
    Ind Past _     => VVPast Indic ;
    Ind Cond Simul => VVPres ;
    Ind Cond Anter =>
       case vvtype of {
          DefVV => VVPast Indic ;
          _     => VVPres } ;
    _ => VVPres } ;

  VV : Type = Verb ** {
      isAux : Bool ;
      compl : VVForm ; -- indicative or subjunctive
      isDef : Bool -- defective verb forms don't get same inflection
      } ;

  VPH : Type = Verb ** {
      comp  : Agr => WordOrder => Str; -- complements of a verb, agr for ReflVP "I/you see myself/yourself" and CompCN "I am human/we are humans"
      vComp : Agr => VVTense => Str; -- when a verb is used as a complement of an auxiliary verb. Unlike ‘comp’ or ‘obj’, this type of complement follows the auxiliary verb.
      obj   : Str ; -- object of a verb; so far only used for A ("paint it black")
      ad    : Str ;
      isNeg : Bool ; -- whether the object is a negative NP: "*there is nothing" -> "there isn't nothing"
      embComp : Str ; -- when a declarative or interrogative sentence is used as a complement of a verb.
      vvtype  : VVType ; -- no VV, fully inflecting VV or defective VV
      } ;

  showVPH = overload {
    showVPH : VerbForm -> Agr -> VPH -> Str = showVPH' OV False VVPres ;
    showVPH : VVTense -> VerbForm -> Agr -> VPH -> Str = showVPH' OV False
  } ;

  showVPHwithImpPrefix = showVPH' OV True VVPres ;
  infVP = overload {
    infVP : VPH -> Str = showVPH' VO False VVPres Inf defaultAgr ; -- word order VO, VerbForm infinitive
    infVP : VerbForm -> VPH -> Str = \vf,vp -> showVPH' VO False VVPres vf defaultAgr vp -- VO as word order, but VerbForm can be any (e.g. present participle or perfect stem)
  } ;

  showVPH' : WordOrder -> Bool -> VVTense -> VerbForm -> Agr -> VPH -> Str =
    \wo,showImpPref,ant,vf,agr,vp ->
       let impPref = case showImpPref of {
         True => vp.s ! ImpPrefix Pos ;
         False => [] }
        in case wo of {
         OV => vp.ad ++ vp.comp ! agr ! wo ++ vp.obj
            ++ vp.prefix ++ impPref ++ vp.s ! vf
            ++ vp.vComp ! agr ! ant ++ vp.embComp ;
         VO => vp.prefix ++ vp.s ! vf ++ vp.ad
            ++ vp.comp ! agr ! wo ++ vp.obj ++ impPref
            ++ vp.vComp ! agr ! ant ++ vp.embComp } ;

  Compl : Type = {s : Str ; ra : Str ; mod : Mod ; isPrep : Bool} ;

  VPHSlash : Type = VPH ** {
    c2 : Compl ;        -- prep or ra for the complement
    agrObj : Agr => Str -- used for SlashV2V
    } ;

  vs : Compl -> {c2 : Compl; agrObj : Agr => Str} = \c ->
    {c2 = c ; agrObj = \\_ => []} ;

  predV : Verb -> VPH = \verb -> verb ** {
    ad,
    obj,
    embComp = [];
    vvtype = NoVV ;
    isNeg = False ;
    comp = \\_,_ => [] ;
    vComp = \\_,_ => [] } ;

   predVc : (Verb ** {c2 : Compl}) -> VPHSlash = \verb ->
    predV verb ** vs verb.c2 ;

  passV : Verb -> VPH = \v -> passVP (predV v) ;

  passVP : VPH -> VPH = \vp -> vp ** {
    s = becomeVerb.s ;
    prefix = case vp.lightverb of {
               Kardan => vp.prefix ;
               _ => vp.s ! PerfStem ++ vp.prefix

             } ;
  } ;
-- ---------------------
-- VP complementation
---------------------
  appCompVP : Compl -> (Mod=>Str) -> (WordOrder=>Str) = \c2,obj ->
    \\wo => let ra = case wo of {VO => [] ; OV => c2.ra} in
     case c2.mod of {
        Ezafe => runtimeKasre c2.s ++ obj ! Bare   ++ ra ;
        _     =>              c2.s ++ obj ! c2.mod ++ ra } ;

  -- for use outside VP, word order is redundant, ra should be retained.
  appComp : Compl -> (Mod=>Str) -> Str = \c2,obj -> appCompVP c2 obj ! OV ;

  insertComp : (Agr => Str) -> VPH -> VPH = \obj,vp -> vp ** {
    comp = \\a,wo => vp.comp ! a ! wo ++ obj ! a
    } ;

  insertCompPre : (Agr=>Mod=>Str) -> VPHSlash -> VPH = \obj,vp -> vp ** {
    comp = \\a,wo => appCompVP vp.c2 (obj ! a) ! wo ++ vp.comp ! a ! wo
    } ;

  insertCompPost : (Agr=>Mod=>Str) -> VPHSlash -> VPH = \obj,vp -> vp ** {
    comp = \\a,wo =>  vp.comp ! a ! wo ++ appCompVP vp.c2 (obj ! a) ! wo
    } ;

  insertVV : VV -> VPH -> VPH = \vv,vp -> predV vv ** {
    vComp = \\a,t => vp.vComp ! a ! t ++ complVV vv vp ! a ! t ;
    vvtype = case vv.isDef of {True => DefVV ; _ => FullVV} ;
  } ;

  embComp : Str -> VPH -> VPH = \str,vp -> vp ** {
    embComp = vp.embComp ++ str ;
    } ;

  insertObj : Str -> VPH -> VPH = \str,vp -> vp ** {
    obj = vp.obj ++ str
    } ;

  complSlash : VPHSlash -> NP -> VPH = \vp,np -> vp ** {
    comp = \\a,wo =>
        case <np.isClitic,vp.c2.isPrep> of {
          <True,False> => [] ; -- clitic is attached to the verb or prefix
          <True,True> => appCompVP vp.c2 (\\_ => BIND ++ np.clitic) ! wo ++ vp.comp ! a ! wo ;
           _ => let nps : Mod=>Str = np.s ;
          --    case np.takesYeAsComp of { True => replaceBare Clitic np.s ; -- TODO: later make a better rule, related to definiteness, when NP takes a clitic.
          --                               _    => np.s } ;
               in appCompVP vp.c2 nps ! wo ++ vp.comp ! a ! wo

        } ;
    s = case <np.isClitic,vp.c2.isPrep> of {
          <True,False> -- if it has no prep, the clitic is attached to the verb (or prefix, if it's a light verb).
            => (addClitic vp.lightverb np.clitic vp).s ;
          _ => vp.s
        } ;
    obj = vp.obj ++ vp.agrObj ! np.a ; -- "beg her to buy", buy agrees with her
    isNeg = np.isNeg
  } ;

---- AR 14/9/2017 trying to fix isAux = True case by inserting conjThat
---- but don't know yet how False should be affect
  complVV : VV -> VPH -> (Agr => VVTense => Str) = \vv,vp ->
    \\agr,ant => if_then_Str vv.isAux conjThat [] ++
      case <ant,vv.isDef,vv.compl> of {
       -- Auxiliaries with defective inflection: complement inflects in tense
        <VVPast Indic,True,>  => showVPHwithImpPrefix (VPast Pos agr) agr vp ;
        <VVPast Indic,_,_>    => showVPH (VPast Pos agr) agr vp ;
        <VVPast Subj>         => showVPH PerfStem agr vp ++ subjAux Pos agr ;

        -- Auxiliaries that take indicative (full or defective inflection)
        <VVPres,_,Indic> => showVPH (VAor Pos agr) agr vp ;

       -- Default: complement in subjunctive
        _ => showVPH (VSubj Pos agr) agr vp
    } ;

  insertAdv : Str -> VPH -> VPH = \ad,vp -> vp ** {
    ad = vp.ad ++ ad ;
  } ;

  insertAdV : Str -> VPH -> VPH = \ad,vp -> vp ** {
    ad = ad ++ vp.ad ;
  } ;

  conjThat : Str = "که" ;

---------------------------
--- Clauses
---------------------------
  Clause : Type = {s : TAnt => Polarity => Order => Str} ;
  SlClause : Type = {quest : Order => Str ; subj : Str ; vp : TAnt => Polarity => Order => Str} ;
---- AR 18/9/2017 intermediate SClause to preserve SOV in e.g. QuestionPes.QuestSlash

  clTable : VPH -> (Agr => TAnt => Polarity => Str) = \vp ->
    \\agr,vt,pol => vp.prefix ++ case vt of {
      Ind Pres Simul => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ; -- for reg. verbs, VAor pol is invariant and negation comes in ImpPrefix.
      Ind Pres Anter => vp.s ! VPerf pol agr ;
      Ind Past Simul => vp.s ! VPast pol agr ; -- Past Simul: simple past
      Ind Past Anter | Ind Cond _ =>   -- Past Anter & Cond _: continuous past
        case vp.vvtype of {
          DefVV => vp.s ! VPast pol agr ;
          _ => vp.s ! ImpPrefix pol ++ vp.s ! VPast Pos agr } ;
      Ind Fut  Simul =>
         case vp.vvtype of {
           DefVV => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ;
           _ => futAux pol agr ++ vp.s ! PastStem
         } ; -- PastStem is, despite the name, used for future too. /IL
      Ind Fut  Anter =>
         case vp.vvtype of {
           DefVV => vp.s ! VPerf pol agr ;
           _ => futAux pol agr ++ vp.s ! PastStem } ;
      Sub _ => vp.s ! VSubj pol agr -- TODO: anterior subjunctive ?
  } ;

  mkClause : NP -> VPH -> Clause = \np,vp ->
    let cls = mkSlClause np vp
    in {s = \\vt,b,ord => cls.quest ! ord ++ cls.subj ++ cls.vp ! vt ! b ! ord} ;

  mkSlClause : NP -> VPH -> SlClause = \np,vp -> {
    quest = table
              { ODir => [];
                OQuest => "آیا" } ;
    subj = np2str np ;
    vp = \\ta,p,ord =>
      let pol = case orB np.isNeg vp.isNeg of {True => Neg ; _ => p} ;
          vps = clTable vp ! np.a ! ta ! pol ;
          vvt = ta2vvt ta vp.vvtype ;
       in case vp.vvtype of {
            DefVV
              => vps ++ vp.ad ++ vp.comp ! np.a ! OV ++ vp.obj
              ++ vp.vComp ! np.a ! vvt ++ vp.embComp ;
            _ => vp.ad ++ vp.comp ! np.a ! OV ++ vp.obj ++ vps
              ++ vp.vComp ! np.a ! vvt ++ vp.embComp }
  };

--Clause : Type = {s : TAnt => Polarity => Order => Str} ;
  mkSClause : Str -> Agr -> VPH -> Clause = \subj,agr,vp -> {
    s = \\ta,p,ord =>
      let pol = case vp.isNeg of {True => Neg ; _ => p} ;
          vps = clTable vp ! agr ! ta ! pol ;
          quest = case ord of { ODir => [] ; OQuest => "آیا" } ;
          vvt = ta2vvt ta vp.vvtype ;
       in quest ++ subj ++ vp.ad ++ vp.comp ! agr ! OV ++ vp.obj
       ++ vps ++ vp.vComp ! agr ! vvt ++ vp.embComp
  };

  predProg : VPH -> VPH = \verb -> verb ** {
    s = \\vh => case vh of {
      ImpPrefix _ => [] ;
      VAor  p a => haveVerb.s ! VAor  Pos a ++ verb.s ! ImpPrefix p ++ verb.s ! VAor Pos a ;
      VPast p a => haveVerb.s ! VPast Pos a ++ verb.s ! ImpPrefix p ++ verb.s ! VPast Pos a ; -- negation in ImpPrefix
	    _ => verb.s ! vh } ; -- TODO more forms
    } ;

  IndefArticle : Str ;
  IndefArticle = "یک";
  taryn : Str ;
  taryn = "ترین" ;

-----------------------------
-- Noun phrase
-----------------------------

 partNP : Verb -> Str = \v -> v.prefix ++ v.s ! PerfStem ++ "شده" ;

-----------------------------------
-- Reflexive pronouns
-----------------------------------

  reflPron : Agr => Mod => Str = table {
    Ag Sg P1 => modTable "خودم" ;
    Ag Sg P2 => modTable "خودت" ;
    Ag Sg P3 => modTable "خودش" ;
    Ag Pl P1 => modTable "خودمان" ;
    Ag Pl P2 => modTable "خودتان" ;
    Ag Pl P3 => modTable "خودشان"
    } ;

  getPron : Animacy -> Number -> Str = \ani,number ->
   case <ani,number> of {
    <Animate,Sg> => "او" ;
    <Animate,Pl> => zwnj "آن" "ها" ;
    <Inanimate,Sg> => "آن" ;
    <Inanimate,Pl> => zwnj "آن" "ها"
   };

-----------------------------------
-- Personal pronouns
-----------------------------------

  Pron : Type = {s : Str ; ps : Str ; a : Agr} ;

  mkPron : (nom:Str) -> (poss:Str) -> Number -> Person -> Pron -- Hidden from public API, confusing naming. /IL
    = \nom,poss,nn,p -> lin Pron {s = nom ; a = Ag nn p ; ps = poss};


  agr2pron : Agr => Pron = table {
    Ag Sg P1 => mkPron "من"   "م" Sg P1 ;
    Ag Sg P2 => mkPron "تو"   "ت" Sg P2 ;
    Ag Sg P3 => mkPron "او"   "ش"  Sg P3 ;
    Ag Pl P1 => mkPron "ما"   "مان" Pl P1 ;
    Ag Pl P2 => mkPron "شما"  "تان" Pl P2 ;
    Ag Pl P3 => mkPron (zwnj "آن" "ها")  "شان" Pl P3
    } ;

}
