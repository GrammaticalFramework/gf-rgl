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
    PMood = Del | Imper | PCond ;

    CardOrd = NCard | NOrd ;
    RAgr = RNoAg | RAg Agr ;

  oper
    CN : Type = Noun ** {
      hasAdj : Bool ;    -- to get the right form when CN is a predicate
      compl : Number => Str -- to make possessive suffix attach to the right word
                            -- dep. on Number because of RelCN
      } ;

    NP : Type = {
      s : Mod => Str ; -- NP can appear with a clitic, need to keep Mod open
      a : Agr ;
      hasAdj : Bool ; -- to get the right form when NP is a predicate
      compl : Str ;   -- to make possessive suffix attach to the right word
      animacy : Animacy -- to get the right relative pronoun
      } ;

  oper
    emptyNP : NP = {
      s = \\_ => [] ;
      a = defaultAgr ;
      hasAdj = False ;
      animacy = Inanimate ;
      compl = []
      } ;

    useN : Noun -> CN = \n -> n ** {
      hasAdj = False ;
      compl = \\_ => []
    } ;

    np2str : NP -> Str = \np ->
      np.s ! Bare ++ np.compl ;

    cn2str : CN -> Str = \cn ->
      cn.s ! Sg ! Bare ++ cn.compl ! Sg ;

 -----------------------
 --- Verb Phrase
 -----------------------
param
  VVForm = Indic | Subj ;
  VVTense = VVPres | VVPerf | VVPast ; -- VVPast Anteriority ???

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
      subj  : VType ;
      ad    : Str ;
      embComp : Str ; -- when a declarative or interrogative sentence is used as a complement of a verb.
      isVV  : Bool ; -- whether a VV has been added
      isDef : Bool ; -- whether a the VV is defective
      } ;

  showVPH = overload {
    showVPH : VerbForm -> Agr -> VPH -> Str = showVPH' VVPres ;
    showVPH : VVTense -> VerbForm -> Agr -> VPH -> Str = showVPH'
  } ;

  showVPH' : VVTense -> VerbForm -> Agr -> VPH -> Str =
    \ant,vf,agr,vp -> vp.ad ++ vp.comp ! agr ++ vp.obj
                   ++ vp.prefix ++ vp.s ! vf
                   ++ vp.vComp ! agr ! ant ++ vp.embComp ;

  Compl : Type = {s : Str ; ra : Str} ;

  VPHSlash : Type = VPH ** {c2 : Compl} ;

 param
    TAnt = TA Tense Anteriority ;

    VType = VIntrans | VTrans | VTransPost ; -- TODO: find out if needed

oper

  predV : Verb -> VPH = \verb -> verb ** {
    subj = VIntrans ;
    ad,
    obj,
    embComp = [];
    isDef,isVV = False ;
    comp = \\_ => [] ;
    vComp = \\_,_ => [] } ;

   predVc : (Verb ** {c2,c1 : Str}) -> VPHSlash = \verb ->
    predV verb ** {c2 = {s = verb.c1 ; ra = []} } ;

---------------------
-- VP complementation
---------------------
  appComp : Compl -> Str -> Str = \c2,obj ->
    c2.s ++ obj ++ c2.ra ;

  insertComp : (Agr => Str) -> VPH -> VPH = \obj,vp -> vp ** {
    comp = \\a => vp.comp ! a ++ obj ! a
    } ;

  insertCompPre : (Agr=>Str) -> VPHSlash -> VPH = \obj,vp -> vp ** {
    comp = \\a => appComp vp.c2 (obj ! a) ++ vp.comp ! a
    } ;

  insertVV : VV -> VPH -> VPH = \vv,vp -> predV vv ** {
    vComp = \\a,t => vp.vComp ! a ! t ++ complVV vv vp ! a ! t ;
    isVV = True ;
    isDef = vv.isDef ;
  } ;

  embComp : Str -> VPH -> VPH = \str,vp -> vp ** {
    embComp = vp.embComp ++ str ;
    } ;

  insertObj : Str -> VPH -> VPH = \str,vp -> vp ** {
    obj = vp.obj ++ str
    } ;

  complSlash : VPHSlash -> NP -> VPH = \vp,np -> vp ** {
    comp = \\a => appComp vp.c2 (np.s ! Bare) ++ np.compl ++ vp.comp ! a
  } ;

---- AR 14/9/2017 trying to fix isAux = True case by inserting conjThat
---- but don't know yet how False should be affect
  complVV : VV -> VPH -> (Agr => VVTense => Str) = \vv,vp ->
    \\agr,ant => if_then_Str vv.isAux conjThat [] ++
      case <ant,vv.compl,vv.isDef> of {
        -- Auxiliaries with full inflection: complement in subjunctive
        <_,_,False>    => showVPH (VSubj Pos agr) agr vp ; --

       -- Auxiliaries with defective inflection: complement inflects in tense
        <VVPres,Subj,True>  => showVPH (VSubj Pos agr) agr vp ;
        <VVPres,Indic,True> => showVPH (VAor Pos agr) agr vp ;
        <VVPast,_,True>       => showVPH (VPast Pos agr) agr vp ;
--        <VVPast Anter> => showVPH PerfStem agr vp ++ pluperfAux Pos agr ; -- TODO do we need this?
        <VVPerf,_,True>       => showVPH PerfStem agr vp ++ subjAux Pos agr ;
        _ => showVPH (VSubj Pos agr) agr vp ---- TODO more forms ?
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
         case vp.isDef of {
           True  => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ;
           False => vp.s ! PerfStem ++ pluperfAux pol agr } ;
      TA Fut  Simul =>
         case vp.isDef of {
           True  => vp.s ! ImpPrefix pol ++ vp.s ! VAor pol agr ;
           False => futAux pol agr ++ vp.s ! PastStem
         } ; -- PastStem is, despite the name, used for future too. /IL
      TA Fut  Anter =>
         case vp.isDef of {
           True  => vp.s ! VPerf pol agr ;
           False => "خواسته" ++ pluperfAux pol agr ++ vp.s ! PastStem
         } ; -- verb form need to be confirmed
      TA Cond Simul => vp.s ! VSubj pol agr ;
      TA Cond Anter =>
         case vp.isDef of {
           True  => vp.s ! VSubj pol agr ;
           False => vp.s ! PerfStem ++ subjAux pol agr } -- verb form to be confirmed
  } ;

  mkClause : NP -> VPH -> Clause = \np,vp ->
    let cls = mkSlClause np vp
    in {s = \\vt,b,ord => cls.quest ! ord ++ cls.subj ++ cls.vp ! vt ! b ! ord} ;

  mkSlClause : NP -> VPH -> SlClause = \np,vp -> {
    quest = table
              { ODir => [];
                OQuest => "آیا" } ;
    subj = np.s !  Bare ;
    vp = \\ta,p,ord =>
      let vps = clTable vp ! np.a ! ta ! p ;
          vvt = ta2vvt ta ;
       in vp.ad ++ vp.comp ! np.a ++ vp.obj ++ vps
       ++ vp.vComp ! np.a ! vvt ++ vp.embComp
  };

--Clause : Type = {s : TAnt => Polarity => Order => Str} ;
  mkSClause : Str -> Agr -> VPH -> Clause = \subj,agr,vp -> {
    s = \\ta,p,ord =>
      let vps = clTable vp ! agr ! ta ! p ;
          quest = case ord of { ODir => [] ; OQuest => "آیا" } ;
          vvt = ta2vvt ta ;
       in quest ++ subj ++ vp.ad ++ vp.comp ! agr ++ vp.obj
       ++ vps ++ vp.vComp ! agr ! vvt ++ vp.embComp
  };

  predProg : VPH -> VPH = \verb -> verb ** {
    s = \\vh => case vh of {
      ImpPrefix _ => [] ;
      VAor  p a => haveVerb.s ! VAor  Pos a ++ verb.s ! ImpPrefix p ++ verb.s ! VAor Pos a ;
      VPast p a => haveVerb.s ! VPast Pos a ++ verb.s ! ImpPrefix p ++ verb.s ! VPast Pos a ; -- negation in ImpPrefix
	    _ => verb.s ! vh } ; -- TODO more forms
    subj = VIntrans
    } ;

  IndefArticle : Str ;
  IndefArticle = "یک";
  taryn : Str ;
  taryn = "ترین" ;

-----------------------------
-- Noun Phrase
-----------------------------

 partNP : Verb -> Str = \v -> v.prefix ++ v.s ! PerfStem ++ "شده" ;

-----------------------------------
-- Reflexive Pronouns
-----------------------------------

  reflPron : Agr => Str = table {
    Ag Sg P1 => "خودم" ;
    Ag Sg P2 => "خودت" ;
    Ag Sg P3 => "خودش" ;
    Ag Pl P1 => "خودمان" ;
    Ag Pl P2 => "خودتان" ;
    Ag Pl P3 => "خودشان"

    } ;

  getPron : Animacy -> Number -> Str = \ani,number ->
   case <ani,number> of {
    <Animate,Sg> => "او" ;
    <Animate,Pl> => zwnj "آن" "ها" ;
    <Inanimate,Sg> => "آن" ;
    <Inanimate,Pl> => zwnj "آن" "ها"
   };

}
