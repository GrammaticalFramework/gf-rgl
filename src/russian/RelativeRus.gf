concrete RelativeRus of Relative = CatRus ** open
  ParadigmsRus,
  ResRus,
  MorphoRus,
  Prelude, Coordination in {

lin
  -- : Cl -> RCl ;            -- such that John loves her
  RelCl cl = {
    subj=such.s ;
    adv=\\a=> comma ++ "что" ++ cl.adv ;  -- TODO: this should be after subj in this case
    verb=cl.verb ;
    dep=cl.dep ;
    compl=\\a=>cl.compl ;
    a=cl.a
    } ;
  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls = {
    subj=\\gn,anim,cas => cls.c.s ++ (rp.s ! gn ! Inanimate ! cls.c.c) ++ cls.subj ;
    adv=\\a=>cls.adv ;
    verb=cls.verb ;
    dep=cls.dep ;
    compl=\\a=> cls.compl ;
    a=cls.a
    } ;

  -- : RP -> VP -> RCl ;      -- who loves John
  RelVP rp vp = {
    subj=rp.s;
    adv=\\a=>vp.adv ! Ag (GSg Neut) P3 ;
    verb=vp.verb ;
    dep=vp.dep ;
    compl=vp.compl ;
    a=rp.a
    } ;

  -- : RP ;                      -- which
  IdRP = lin RP (doKotoryjPron "который" (Ag (GSg Neut) P3)) ;

  -- Prep -> NP -> RP -> RP ;  -- the mother of whom
  FunRP prep np rp = {
    s=\\gn,anim,cas => np.s ! cas ++ prep.s ++ rp.s ! gn ! Inanimate ! prep.c ;
    a=np.a
    } ;
}
