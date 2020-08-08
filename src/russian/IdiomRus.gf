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
  ExistNP np = {subj=np.s ! Nom ; compl="" ; verb=to_exist ; dep=[] ; adv=[] ; a=np.a} ;

  -- : IP -> QCl ;       -- which houses are there
  ExistIP ip = {
    subj=ip.nom ; -- gen?
    adv=[] ;
    verb=to_exist;
    dep=[] ;
    compl=[];
    a=ip.a
    } ;
  -- : NP -> Adv -> Cl ;    -- there is a house in Paris
  ExistNPAdv np adv = {
    subj=np.s ! Nom ;
    compl="" ;
    verb=to_exist ;
    dep=[] ;
    adv=adv.s ;
    a=np.a
    } ;
  -- : IP -> Adv -> QCl ;   -- which houses are there in Paris
  ExistIPAdv ip adv = {
    subj=ip.nom ; -- gen?
    adv=adv.s ;
    verb=to_exist;
    dep=[] ;
    compl=[];
    a=ip.a
    } ;

  -- : VP -> VP ;        -- be sleeping
  ProgrVP vp = vp ;
  -- : VP -> Utt ;       -- let's go
  ImpPl1 vp =
    let a = Ag GPl P1 in {
      s = (verbEnvAgr "давайте" (vp.adv ! a) vp.verb Infinitive Pres a PPos) ++ vp.dep ++ vp.compl ! a
      } ;
  -- : NP -> VP -> Utt ; -- let John walk
  ImpP3 np vp = {
    s = (verbEnvAgr "пусть" (vp.adv ! np.a ++ np.s ! Nom) vp.verb Ind Pres np.a PPos) ++ vp.dep ++ vp.compl ! np.a
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
