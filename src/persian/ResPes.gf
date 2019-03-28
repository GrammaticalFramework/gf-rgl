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
      hasAdj : Bool ; -- to get the right form when NP is a predicate
      animacy : Animacy ; -- to get the right pronoun in FunRP
      relpron : RelPron ; -- contraction for "that which"
      empty : Str -- to prevent metavariables in case of rel.pron. contraction
      } ;

  oper
    emptyNP : NP = {
      s = \\_ => [] ;
      a = defaultAgr ;
      hasAdj = False ;
      animacy = Inanimate ;
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
  VVForm = Indic | Subj ;
  VVTense = VVPres | VVPerf | VVPast ; -- VVPast Anteriority ???
  TAnt = TA Tense Anteriority ;

oper

  -- TODO: all forms
  ta2vvt : TAnt -> VVTense = \ta -> case ta of {
    TA Pres Anter => VVPerf ;
    TA Past _     => VVPast ;
    _ => VVPres } ;

  VV : Type = Verb ** {
      isAux : Bool ;
      compl : VVForm ; -- indicative or subjunctive
      isDef : Bool -- defective verb forms don't get same inflection
      } ;

  VPH : Type = Verb ** {
      comp  : Agr => Str; -- complements of a verb, agr for ReflVP "I/you see myself/yourself" and CompCN "I am human/we are humans"
      vComp : Agr => VVTense => Str; -- when a verb is used as a complement of an auxiliary verb. Unlike ‘comp’ or ‘obj’, this type of complement follows the auxiliary verb.
      obj   : Str ; -- object of a verb; so far only used for A ("paint it black")
      ad    : Str ;
      embComp : Str ; -- when a declarative or interrogative sentence is used as a complement of a verb.
      vvtype  : VVType ; -- no VV, fully inflecting VV or defective VV
      } ;

  showVPH = overload {
    showVPH : VerbForm -> Agr -> VPH -> Str = showVPH' VVPres ;
    showVPH : VVTense -> VerbForm -> Agr -> VPH -> Str = showVPH'
  } ;

  showVPH' : VVTense -> VerbForm -> Agr -> VPH -> Str =
    \ant,vf,agr,vp -> vp.ad ++ vp.comp ! agr ++ vp.obj
                   ++ vp.prefix ++ vp.s ! vf
                   ++ vp.vComp ! agr ! ant ++ vp.embComp ;

  -- A hack: we reuse the obj field for the VP complement in
  -- SlashV2V and this is needed to get the right word order for complVV.
  showVPHvv : VerbForm -> Agr -> VPH -> Str = \vf,agr,vp ->
      vp.comp ! agr ++ vp.prefix ++ vp.s ! vf  -- vp.ad is missing on purpose! we add it in insertVV.
   ++ vp.obj ++ vp.vComp ! agr ! VVPres ++ vp.embComp ;

  Compl : Type = {s : Str ; ra : Str ; mod : Mod} ;

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
    comp = \\_ => [] ;
    vComp = \\_,_ => [] } ;

   predVc : (Verb ** {c2 : Compl}) -> VPHSlash = \verb ->
    predV verb ** vs verb.c2 ;

---------------------
-- VP complementation
---------------------
  appComp : Compl -> (Mod=>Str) -> Str = \c2,obj ->
    c2.s ++ obj ! c2.mod ++ c2.ra ;

  insertComp : (Agr => Str) -> VPH -> VPH = \obj,vp -> vp ** {
    comp = \\a => vp.comp ! a ++ obj ! a
    } ;

  insertCompPre : (Agr=>Mod=>Str) -> VPHSlash -> VPH = \obj,vp -> vp ** {
    comp = \\a => appComp vp.c2 (obj ! a) ++ vp.comp ! a
    } ;

  insertCompPost : (Agr=>Mod=>Str) -> VPHSlash -> VPH = \obj,vp -> vp ** {
    comp = \\a =>  vp.comp ! a ++ appComp vp.c2 (obj ! a)
    } ;

  insertVV : VV -> VPH -> VPH = \vv,vp -> predV vv ** {
    vComp = \\a,t => vp.vComp ! a ! t ++ complVV vv vp ! a ! t ;
    vvtype = case vv.isDef of {True => DefVV ; _ => FullVV} ;
    ad = vp.ad  -- because complVV doesn't include ad! for word order.
  } ;

  embComp : Str -> VPH -> VPH = \str,vp -> vp ** {
    embComp = vp.embComp ++ str ;
    } ;

  insertObj : Str -> VPH -> VPH = \str,vp -> vp ** {
    obj = vp.obj ++ str
    } ;

  complSlash : VPHSlash -> NP -> VPH = \vp,np -> vp ** {
    comp = \\a => appComp vp.c2 np.s ++ vp.comp ! a ;
    obj = vp.obj ++ vp.agrObj ! np.a -- "beg her to buy", buy agrees with her
  } ;

---- AR 14/9/2017 trying to fix isAux = True case by inserting conjThat
---- but don't know yet how False should be affect
  complVV : VV -> VPH -> (Agr => VVTense => Str) = \vv,vp ->
    \\agr,ant => if_then_Str vv.isAux conjThat [] ++
      case <ant,vv.isDef,vv.compl> of {
       -- Auxiliaries with defective inflection: complement inflects in tense
        <VVPast,True,_> => showVPHvv (VPast Pos agr) agr vp ;
--        <VVPast Anter> => showVPH PerfStem agr vp ++ pluperfAux Pos agr ; -- TODO do we need this?
        <VVPerf,True,_> => showVPHvv PerfStem agr vp ++ subjAux Pos agr ;

        -- Auxiliaries that take indicative (full or defective inflection)
        <VVPres,_,Indic> => showVPHvv (VAor Pos agr) agr vp ;

       -- Default: complement in subjunctive
        _ => showVPHvv (VSubj Pos agr) agr vp ---- TODO more forms ?
    } ;

  insertAdV : Str -> VPH -> VPH = \ad,vp -> vp ** {
    ad = vp.ad ++ ad ;
  } ;

  conjThat : Str = "که" ;

---------------------------
--- Clauses
---------------------------
  Clause : Type = {s : TAnt => Polarity => Order => Str} ;
  SlClause : Type = {quest : Order => Str ; subj : Str ; vp : TAnt => Polarity => Order => Str} ;
---- AR 18/9/2017 intermediate SClause to preserve SOV in e.g. QuestionPes.QuestSlash

 -- TODO: check the VV forms with defective verbs
  clTable : VPH -> (Agr => TAnt => Polarity => Str) = \vp ->
    \\agr,vt,pol => vp.prefix ++ case vt of {
      TA Pres Simul => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ; -- for reg. verbs, VAor pol is invariant and negation comes in ImpPrefix.
      TA Pres Anter => vp.s ! VPerf pol agr ;
      TA Past Simul => vp.s ! VPast pol agr ;
      TA Past Anter =>
         case vp.vvtype of {
           DefVV => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ;
           _ => vp.s ! PerfStem ++ pluperfAux pol agr } ;
      TA Fut  Simul =>
         case vp.vvtype of {
           DefVV => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ;
           _ => futAux pol agr ++ vp.s ! PastStem
         } ; -- PastStem is, despite the name, used for future too. /IL
      TA Fut  Anter =>
         case vp.vvtype of {
           DefVV => vp.s ! VPerf pol agr ;
           _ => "خواسته" ++ pluperfAux pol agr ++ vp.s ! PastStem
         } ; -- verb form need to be confirmed
      TA Cond Simul => vp.s ! VSubj pol agr ;
      TA Cond Anter =>
         case vp.vvtype of {
           DefVV => vp.s ! VSubj pol agr ;
           _ => vp.s ! PerfStem ++ subjAux pol agr } -- verb form to be confirmed
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
      let vps = clTable vp ! np.a ! ta ! p ;
          vvt = ta2vvt ta ;
       in case vp.vvtype of {
            DefVV
              => vps ++ vp.ad ++ vp.comp ! np.a ++ vp.obj
              ++ vp.vComp ! np.a ! vvt ++ vp.embComp ;
            _ => vp.ad ++ vp.comp ! np.a ++ vp.obj ++ vps
              ++ vp.vComp ! np.a ! vvt ++ vp.embComp }
  };

--Clause : Type = {s : TAnt => Polarity => Order => Str} ;
  mkSClause : Str -> Agr -> VPH -> Clause = \subj,agr,vp -> {
    s = \\ta,p,ord =>
      let vps = clTable vp ! agr ! ta ! p ;
          quest = case ord of { ODir => [] ; OQuest => "آیا" } ;
          vvt = ta2vvt ta ;
       in quest ++ subj ++ vp.ad ++ vp.comp ! agr ++ vp.obj
--       in quest ++ vp.ad ++ subj ++ vp.comp ! agr ++ vp.obj -- TODO check which word order is better /IL
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
