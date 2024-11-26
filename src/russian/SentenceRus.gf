concrete SentenceRus of Sentence = CatRus ** open Prelude, TenseRus, ParamRus, Coordination, Maybe, (R=ResRus) in {
flags optimize=all_subs ; coding=utf8 ;
lin
  -- : Adv -> S -> S ;            -- then I will go home
  AdvS adv s = {s=\\m => adv.s ++ s.s ! m} ;
  -- : Adv -> S -> S ;            -- next week, I will go home
  ExtAdvS adv s = {s=\\m => adv.s ++ comma ++ s.s ! m} ;

  -- : Temp -> Pol -> Cl -> S ;   -- she had not slept - она не спала
  UseCl temp pol cl = {
    s : Mood => Str = \\m =>
      temp.s
      ++ (R.verbEnvAgr cl.subj cl.adv cl.verb m temp.t cl.a pol)
      ++ cl.dep
      ++ cl.compl ! pol.p ;
    } ;

  -- : Temp -> Pol -> RCl -> RS ;  -- that had not slept
  UseRCl temp pol rcl = {
    s = \\gn,anim,cas =>
      let a : Agr = fromMaybe Agr (genNumAgrP3 gn) rcl.a in
       temp.s
       ++ (R.verbEnvAgr (rcl.subj ! gn ! anim ! Nom) (rcl.adv ! (genNumAgrP3 gn)) rcl.verb Ind temp.t a pol)
       ++ rcl.dep
       ++ rcl.compl ! pol.p ! (genNumAgrP3 gn)
    } ;

  -- : Temp -> Pol -> ClSlash -> SSlash ; -- (that) she had not seen
  UseSlash temp pol cls = {
    s : Mood => Str = \\m =>
      temp.s
      ++ (R.verbEnvAgr cls.subj cls.adv cls.verb m temp.t cls.a pol)
      ++ cls.dep
      ++ cls.compl ! pol.p ;
    c=cls.c
    } ;

  -- : NP -> VPSlash -> ClSlash ;      -- (whom) he sees
  SlashVP np vps = {
    subj=np.s ! Nom ; -- ????????
    adv=vps.adv ! np.a ;  -- ??
    verb=vps.verb ;
    dep=vps.dep ;
    compl=\\p => vps.compl1 ! p ! np.a ++ vps.compl2 ! p ! np.a;  -- ??
    a=np.a ;
    c=vps.c
  } ;

  -- : Cl -> Prep -> ClSlash ;         -- (with whom) he walks
  SlashPrep cl prep = {
    subj=cl.subj ;
    compl=cl.compl ;
    adv=cl.adv ;
    verb=cl.verb ;
    dep=cl.dep ;
    a=cl.a ;
    c=prep
    } ;

  -- NP -> VS -> SSlash -> ClSlash ; -- (whom) she says that he loves
  SlashVS np vs ss = {
    subj=np.s ! Nom  ; -- ????????
    compl=\\_ => embedInCommas ("что" ++ ss.s ! Ind) ;  -- ?? that?
    adv=[];  -- ??
    verb=vs ;
    dep=[] ;
    a=np.a ;
    c=ss.c
  } ;

  -- : S -> Subj -> S -> S ;       -- I go home, if she comes
  SSubjS s subj s2 = {
    s=\\m => s.s ! m ++ comma ++ subj.s ++ s2.s ! Ind
    } ;

  -- : ClSlash -> Adv -> ClSlash ;     -- (whom) he sees today
  AdvSlash cls adv = cls ** {
    adv=cls.adv ++ adv.s
    } ;

  -- : VP -> Imp ;             -- love yourselves
  ImpVP vp = {
    s = \\polarity, gn =>
      let pol = case (R.orPol vp.p polarity) of {Neg => PNeg; Pos => PPos} in
      let a = Ag gn P2 in
        (R.verbEnvAgr "" "" vp.verb Imperative Pres a pol)
        ++ vp.dep
        ++ vp.adv ! a
        ++ vp.compl ! pol.p ! a
    } ;

  -- : NP -> VP -> Cl ;         -- John walks - Иван гуляет
  PredVP np vp = {
    subj=np.s ! Nom ;
    adv=vp.adv ! np.a ;
    verb=vp.verb ;
    dep=vp.dep ;
    compl=\\p => vp.compl ! p ! np.a ;
    a=np.a
    } ;

  -- : SC -> VP -> Cl ;         -- that she goes is good - что она идёт есть хорошо
  PredSCVP sc vp = {
    subj=sc.s ;
    adv=vp.adv ! (Ag (GSg Neut) P3) ;  -- ???
    verb=vp.verb ;
    dep=vp.dep ;
    compl=\\p => vp.compl ! p ! Ag (GSg Neut) P3 ;  -- ???
    a=Ag (GSg Neut) P3   -- ???
    } ;

  -- : S -> SC ;               -- that she goes - что она идёт
  EmbedS s = {s = "что" ++ s.s ! Ind} ;

  -- : Temp -> Pol -> QCl -> QS ;  -- who had not slept
  UseQCl temp pol cl = {
    s = \\qf =>
      temp.s
      ++ (R.verbEnvAgr cl.subj cl.adv cl.verb Ind temp.t cl.a pol)
      ++ cl.dep
      ++ cl.compl ! pol.p
    } ;

  -- : QS -> SC ;               -- who goes
  EmbedQS qs = {s = qs.s ! QIndir} ;

  -- : VP -> SC ;               -- to go
  EmbedVP vp = {
    s=vp.adv ! Ag (GSg Neut) P3
      ++ (R.verbInf vp.verb)
      ++ vp.dep
      ++ vp.compl ! Pos ! Ag (GSg Neut) P3    -- ???
    } ;

  -- : S -> RS -> S ;              -- she sleeps, which is good
  RelS s rs = {
    s=\\m=>s.s ! m ++ comma ++ rs.s ! GSg Neut ! Inanimate ! Nom ;
    } ;
}
