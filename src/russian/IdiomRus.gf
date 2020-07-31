--# -path=.:../abstract:../common:../../prelude

concrete IdiomRus of Idiom = CatRus ** open Prelude, ParamX, TenseRus, ResRus, Coordination, MorphoRus in {
flags optimize=all_subs ;  coding=utf8 ;

lin
  -- : VP -> Cl ;        -- it is hot
  ImpersCl vp = let a = Ag (GSg Neut) P3 in {subj="" ; compl=vp.compl ! a ; verb=vp.verb ; dep=vp.dep ; adv=vp.adv ! a ; a=a } ;

  -- : VP -> Cl ;        -- one sleeps
  GenericCl vp = let a = Ag (GSg Masc) P2 in {subj="" ; compl=vp.compl ! a ; verb=vp.verb ; dep=vp.dep ; adv=vp.adv ! a; a=a } ;

  -- : NP -> RS -> Cl ; -- it is I who did it
  CleftNP np rs = {
    subj="это" ++ np.s ! Nom ;
    adv=[];
    verb=copulaEll ;   -- ???
    dep=[] ;
    compl=embedInCommas (rs.s ! agrGenNum np.a ! Animate ! Nom) ;  -- TODO: here or in subj???
    a=np.a
    } ;
  -- : Adv -> S -> Cl ; -- it is here she slept
  CleftAdv adv s = {
    subj="это" ;
    adv=adv.s ;
    verb=nullVerb ;   -- ???
    dep=[] ;
    compl=s.s ! Ind ;
    a=Ag (GSg Neut) P3
    } ;

  -- : NP -> Cl ;        -- there is a house
  ExistNP np = {subj=np.s ! Nom ; compl="" ; verb=to_exist ; dep=[] ; adv=[] ; a=np.a} ;  -- TODO: Different order?

  -- : IP -> QCl ;       -- which houses are there
  ExistIP ip = {
    subj=ip.nom ; -- gen?
    adv=[] ;
    verb=to_exist;
    dep=[] ;
    compl=[];
    a=ip.a
    } ;
  -- TODO: ExistNPAdv : NP -> Adv -> Cl ;    -- there is a house in Paris
  -- TODO: ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris
  -- : VP -> VP ;        -- be sleeping
  ProgrVP vp = vp ;
  -- : VP -> Utt ;       -- let's go
  ImpPl1 vp =
    let a = Ag GPl P1 in
    let pol = PPos in
    let parts = verbAgr vp.verb Infinitive Pres a pol.p in    -- colloquial, should be Fut, but then present fails...
    let p1 = "давайте" in {
      s = p1 ++ pol.s ++ vp.adv ! a ++ parts.p2 ++ vp.dep ++ vp.compl ! a
      } ;
  -- : NP -> VP -> Utt ; -- let John walk
  ImpP3 np vp =
    let pol = PPos in
    let parts = verbAgr vp.verb Ind Pres np.a pol.p in
    let p1 = "пусть" in {
      s = p1 ++ pol.s ++ vp.adv ! np.a ++ np.s ! Nom ++ parts.p2 ++ vp.dep ++ vp.compl ! np.a
      } ;

  -- : VP -> VP ;        -- is at home himself
  SelfAdvVP vp = vp ** {compl=\\a => vp.compl ! a ++ (adjFormsAdjective sam).s ! agrGenNum a ! Animate ! Nom} ;
  -- : VP -> VP ;        -- is himself at home
  SelfAdVVP vp = vp ** {adv=\\a => (adjFormsAdjective sam).s ! agrGenNum a ! Animate ! Nom ++ vp.adv ! a} ;
  -- : NP -> NP ;        -- the president himself (is at home)
  SelfNP np = np ** {
    s=\\cas => ((adjFormsAdjective sam).s ! agrGenNum np.a ! Animate ! cas ) ++ np.s ! cas
    } ;

}
