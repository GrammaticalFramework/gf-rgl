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

oper

  VPH : Type = {
      s     : VPHForm => Str ;
      comp  : Agr => Str; -- complements of a verb, agr for e.g. CompCN "I am human/we are humans"
      vComp : Agr => Str; -- when a verb is used as a complement of an auxiliary verb. Unlike ‘comp’ or ‘obj’, this type of complement follows the auxiliary verb.
      obj   : Str ; -- object of a verb; so far only used for A ("paint it black")
      subj  : VType ;
      ad    : Str ;
      embComp : Str ; -- when a declarative or interrogative sentence is used as a complement of a verb.
      wish : Bool ; -- whether a VV has been added
      } ;

  showVPH : VPHForm -> Agr -> VPH -> Str = \vf,agr,vp ->
    vp.ad ++ vp.comp ! agr ++ vp.obj ++ vp.s ! vf ++ vp.vComp ! agr ++ vp.embComp ;

  Compl : Type = {s : Str ; ra : Str} ;

  VPHSlash : Type = VPH ** {c2 : Compl} ;

 param

    VPHForm =
       VPTense Polarity VPPTense Agr -- 9 * 12
--     | VPReq
       | VPImp Polarity Number
--     | VPReqFut
     | VVForm Agr
     | VPStem1
     | VPStem2
     | VPInf
     ;

    VPHTense =
       VPres  -- impf hum       nahim    "I گْ"
     | VPast  -- impf Ta        nahim    "I weنت"
     | VFut      -- fut            na/nahim "I سهلل گْ"
     | VPerfPres -- perf hum       na/nahim "I هوe گْنe"
     | VPerfPast -- perf Ta        na/nahim "I هد گْنe"
     | VPerfFut
     | VCondSimul
     | VCondAnter -- subj           na       "I می گْ"
     | VVVForm -- AR 21/3/2018 for mustCl after Nasrin
     | VRoot1  -- AR 22/3/2018 for mustCl past after Nasrin
    ;

    VType = VIntrans | VTrans | VTransPost ;

 VPPTense =
	  VPPres Anteriority
	  |VPPast Anteriority
	  |VPFutr Anteriority
	  |VPCond Anteriority ;
oper

  predV : Verb -> VPH = \verb -> {
    s = \\vh =>
     case vh of {
       VPTense pol (VPPres Simul) agr => verb.s ! VF pol (PPresent2 PrImperf) agr ;
       VPTense pol (VPPres Anter) agr => verb.s ! VF pol (PPresent2 PrPerf) agr ;
       VPTense pol (VPPast Simul) agr => verb.s ! VF pol (PPast2 PstAorist) agr ;
       VPTense pol (VPPast Anter) agr => verb.s ! VF pol (PPast2 PstPerf) agr ;
       VPTense pol (VPFutr Simul) agr => verb.s ! VF pol (PFut2 FtAorist) agr ;
       VPTense pol (VPFutr Anter) agr => verb.s ! VF pol (PPresent2 PrPerf) agr ; -- this is to be confirmed
       VPTense pol (VPCond Simul) agr => verb.s ! VF pol (PPast2 PstImperf) agr ;
       VPTense pol (VPCond Anter) agr => verb.s ! VF pol (PPast2 PstImperf) agr ;
       VVForm agr => verb.s ! Vvform agr ;
       VPStem1 => verb.s ! Root1 ;
       VPStem2 => verb.s ! Root2 ;
       VPInf => verb.s ! Inf;
       VPImp pol n =>verb.s ! Imp pol n };
    subj = VIntrans ;
    ad,
    obj,
    embComp = [];
    wish = False ;
    comp,
    vComp = \\_ => [] ;
    } ;

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

  insertVV : (Agr => Str) -> VPH -> VPH = \obj1,vp -> vp ** {
    wish = True ;
    vComp = \\a => vp.comp ! a ++ obj1 ! a ; -- IL why this is vp.comp and not vp.vComp??
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
  infVV : Bool -> VPH -> (Agr => Str) = \isAux,vp ->
    \\agr => if_then_Str isAux conjThat [] ++ showVPH (VVForm agr) agr vp ;

  insertAdV : Str -> VPH -> VPH = \ad,vp -> vp ** {
    ad = vp.ad ++ ad ;
  } ;

  conjThat : Str = "که" ;

---------------------------
--- Clauses
---------------------------
  Clause : Type = {s : VPHTense => Polarity => Order => Str} ;
  SlClause : Type = {quest : Order => Str ; subj : Str ; vp : VPHTense => Polarity => Order => Str} ;

---- AR 18/9/2017 intermediate SClause to preserve SOV in e.g. QuestionPes.QuestSlash

  clTable : VPH -> (Agr => VPHTense => Polarity => Str) = \vp ->
    \\agr,vt,b => case <b,vt> of {
      <Pos,VPres>     => vp.s ! VPTense Pos (VPPres Simul) agr ;
      <Neg,VPres>     => vp.s ! VPTense Neg (VPPres Simul) agr ;
      <Pos,VPerfPres> => vp.s ! VPTense Pos (VPPres Anter) agr ;
      <Neg,VPerfPres> => vp.s ! VPTense Neg (VPPres Anter) agr ;
      <Pos,VPast>     => vp.s !  VPTense Pos (VPPast Simul) agr ;
      <Neg,VPast>     => vp.s !  VPTense Neg (VPPast Simul) agr ;
      <Pos,VPerfPast> => vp.s !  VPTense Pos (VPPast Anter) agr ;
      <Pos,VFut>      => case vp.wish of {
                           True  => vp.s ! VPTense Pos (VPPres Simul) agr ;
                           False => vp.s ! VPTense Pos (VPFutr Simul) agr };
      <Pos,VPerfFut>  => case vp.wish of {
                           True  => vp.s ! VPTense Pos (VPPres Anter) agr ;
                           False => vp.s ! VPTense Pos (VPFutr Anter) agr };  -- verb form need to be confirmed
      <Neg,VPerfPast> => vp.s ! VPTense Neg (VPPast Anter) agr ;
      <Neg,VFut>      => case vp.wish of {
                           True  => vp.s ! VPTense Neg (VPPres Simul) agr ;
                           False => vp.s ! VPTense Neg (VPFutr Simul) agr };
      <Neg,VPerfFut>  => case vp.wish of {
                           True => vp.s ! VPTense Neg (VPPres Anter) agr ;
                           False => vp.s ! VPTense Neg (VPFutr Anter) agr };  -- verb form need to be confirmed
      <Pos,VCondSimul> => vp.s ! VPTense Pos (VPCond Simul) agr ;
      <Pos,VCondAnter> => vp.s ! VPTense Pos (VPCond Anter) agr; -- verb form to be confirmed
      <Neg,VCondSimul> => vp.s ! VPTense Neg (VPCond Simul) agr ;
      <Neg,VCondAnter> => vp.s ! VPTense Neg (VPCond Anter) agr ; -- verb form to be confirmed
      <_,  VVVForm>    => vp.s ! VVForm agr ; -- AR 21/3/2018
      <_,  VRoot1>     => vp.s ! VPStem1 {- ++ Predef.Bind ++ "ه" -}             -- AR 22/3/2018
      };

  mkClause : NP -> VPH -> Clause = \np,vp ->
    let cls = mkSlClause np vp
    in {s = \\vt,b,ord => cls.quest ! ord ++ cls.subj ++ cls.vp ! vt ! b ! ord} ;

  mkSlClause : NP -> VPH -> SlClause = \np,vp -> {
    quest = table
              { ODir => [];
                OQuest => "آیا" } ;
    subj = np.s !  Bare ;
    vp = \\vt,b,ord =>
      let vps = clTable vp ! np.a ! vt ! b
       in vp.ad ++ vp.comp ! np.a ++ vp.obj ++ vps ++ vp.vComp ! np.a ++ vp.embComp
  };

--Clause : Type = {s : VPHTense => Polarity => Order => Str} ;
  mkSClause : Str -> Agr -> VPH -> Clause = \subj,agr,vp -> {
    s = \\vt,b,ord =>
      let vps = clTable vp ! agr ! vt ! b ;
          quest = case ord of { ODir => [] ; OQuest => "آیا" }
       in quest ++ subj ++ vp.ad ++ vp.comp ! agr ++ vp.obj ++ vps ++ vp.vComp ! agr ++ vp.embComp
  };

  ta2vt : Tense -> Anteriority -> VPHTense = \t,a -> case <t,a> of {
    <Pres,Simul> => VPres ;
    <Pres,Anter> => VPerfPres ;
    <Past,Simul> => VPast  ;
    <Past,Anter> => VPerfPast ;
    <Fut,Simul>  => VFut ;
    <Fut,Anter>  => VPerfFut ;
    <Cond,Simul> => VCondSimul ;
    <Cond,Anter> => VCondSimul } ;

  predAux : Aux -> VPH = \verb -> {
    s = \\vh => case vh of {
	     VPTense pol (VPPres Simul) agr => verb.inf ! AX pol (AuxPresent PrImperf) agr ;
	     VPTense pol (VPPres Anter) agr => verb.inf ! AX pol (AuxPresent PrPerf) agr ;
	     VPTense pol (VPPast Simul) agr => verb.inf ! AX pol (AuxPast PstAorist) agr ;
	     VPTense pol (VPPast Anter) agr => verb.inf ! AX pol (AuxPresent PrPerf) agr ;
	     VPTense pol (VPFutr Simul) agr => verb.inf ! AX pol (AuxFut FtAorist) agr ;
	     VPTense pol (VPFutr Anter) agr => verb.inf ! AX pol (AuxFut FtAorist) agr ; -- this is to be confirmed
	     VPTense pol (VPCond Simul) agr => verb.inf ! AX pol (AuxFut FtAorist)  agr ;
	     VPTense pol (VPCond Anter) agr => verb.inf ! AX pol (AuxPast PstImperf)  agr ;
	     VVForm  agr => []; -- to be checked => verb.s ! Vvform agr ;
	     VPStem1 => [];
	     VPStem2 => "بود" ;
       VPInf => "بودن";
	     VPImp _ _ => [] -- need to be confirmed
		 };
	    obj = [] ;
		  subj = VIntrans ;
		  ad = [];
      embComp = [];
	    wish = False ;
      vComp = \\_ => [] ;
      comp = \\_ => []
    } ;

  Aux = {
      inf :  AuxForm => Str ;
    } ;

  auxBe : Aux = {
    inf  =  table {
     AX pol tense ag => mkAux pol tense ag
    } ;
    } ;

  -- TODO: find out how much overlap with beVerb in MorphoPes /IL
  mkAux : Polarity -> AuxTense -> Agr -> Str = \pol,t,ag ->
    let bodh = "بوده" ;
        nbodh = "نبوده" ;
        hast = "هست" ;
        nhast = "نیست" ;
        bod  = "بود" ;
        khah = "خواه" ;
        nbod  = "نبود" ;
        nkhah = "نخواه" ;
        impfSuff : Str -> Str = imperfectSuffix ag ;
        impfSuffD : Str -> Str = imperfectSuffixD ag ;
        perfSuff : Str -> Str = perfectSuffix ag
    in case <pol,t,ag> of {
      <Pos,AuxPresent PrImperf,Ag Sg P3> => "است" ;
      <Pos,AuxPresent PrImperf>       => impfSuff hast ;
      <Pos,AuxPresent PrPerf>         => perfSuff bodh ;

      <Pos,AuxPast PstPerf>   => [] ;
      <Pos,AuxPast PstImperf> => zwnj "می" (impfSuff bod) ;
      <Pos,AuxPast PstAorist> =>            impfSuff bod ;

      <Pos,AuxFut FtAorist> => impfSuffD khah ++ bod ;

    -- negatives
      <Neg,AuxPresent PrImperf> => impfSuff nhast ;
      <Neg,AuxPresent PrPerf>   => perfSuff nbodh ;

      <Neg,AuxPast PstPerf>   => [] ;
      <Neg,AuxPast PstImperf> => zwnj "نمی" (impfSuff bod) ;
      <Neg,AuxPast PstAorist> =>              impfSuff nbod ;

      <Neg,AuxFut FtAorist> => impfSuffD nkhah ++ bod
    } ;

  param
   AuxTense = AuxPresent PrAspect | AuxPast PstAspect | AuxFut FtAspect ;
   AuxForm = AX Polarity AuxTense Agr ;


 oper

  predProg : VPH -> VPH = \verb -> verb ** {
    s = \\vh => case vh of {
	    VPTense pol (VPPres Simul) agr => toHave Pos (PPresent2 PrImperf) agr ++ verb.s ! VPTense pol (VPPres Simul) agr ;
      VPTense pol (VPPast Simul) agr => toHave Pos (PPast2 PstAorist) agr ++ verb.s ! VPTense pol (VPCond Simul) agr ;
      VPTense pol (VPCond Simul) agr => toHave Pos (PPast2 PstAorist) agr ++ verb.s ! VPTense pol (VPCond Simul) agr ;
      VPTense pol (VPCond Anter) agr => toHave Pos (PPast2 PstAorist) agr ++ verb.s ! VPTense pol (VPCond Anter) agr ;
       -- VPTense pol (VPFutr Anter) agr => verb.s ! VPTense pol (VPFutr Anter) agr ; -- this is to be confirmed
	    _ => verb.s ! vh } ;
    subj = VIntrans
    } ;



IndefArticle : Str ;
IndefArticle = "یک";
taryn : Str ;
taryn = "ترین" ;

-----------------------------
-- Noun Phrase
-----------------------------

 partNP : Str -> Str = \str -> (Prelude.glue str "ه") ++ "شده" ;

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
