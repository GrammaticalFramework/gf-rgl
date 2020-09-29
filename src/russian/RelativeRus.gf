concrete RelativeRus of Relative = CatRus ** open
  ParadigmsRus,
  ResRus,
  MorphoRus,
  Maybe,
  Prelude, Coordination in {

lin
  -- : Cl -> RCl ;            -- such that John loves her
  RelCl cl = {
    subj=\\gn,anim,cas => such.s ! gn ! anim ! cas ++ comma ++ "что" ++ cl.subj ;
    adv=\\a => cl.adv ;
    verb=cl.verb ;
    dep=cl.dep ;
    compl=\\p,a => cl.compl ! p ;
    a=Just Agr cl.a
    } ;
  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls = {
    subj=\\gn,anim,cas => cls.c.s ++ (rp.s ! gn ! anim ! cls.c.c) ++ cls.subj ;
    adv=\\a=>cls.adv ;
    verb=cls.verb ;
    dep=cls.dep ;
    compl=\\p,a => cls.compl ! p ;
    a=Just Agr cls.a
    } ;

  -- : RP -> VP -> RCl ;      -- who loves John
  RelVP rp vp = {
    subj=rp.s;
    adv=\\a=>vp.adv ! Ag (GSg Neut) P3 ;
    verb=vp.verb ;
    dep=vp.dep ;
    compl=vp.compl ;
    a=Nothing Agr (Ag (GSg Neut) P3)
    } ;

  -- : RP ;                      -- which
  IdRP = lin RP (doKotoryjPron "который" (Ag (GSg Neut) P3)) ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  FunRP prep np rp = {
    s=\\gn,anim,cas => np.s ! cas ++ prep.s ++ rp.s ! gn ! Inanimate ! prep.c ;
    a=np.a
    } ;
}
