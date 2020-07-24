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
  RelSlash rp cls = let rp_as_adj = rp.poss ** {preferShort=PrefFull;  sf,sm,sn,sp,comp = []} in {
    subj=(adjFormsAdjective rp_as_adj).s ;     -- TODO: cls.c and applyPrep not used?
    adv=\\a=>cls.adv ;  -- TODO: this should be after subj in this case
    verb=cls.verb ;
    dep=cls.dep ;
    compl=\\a=>cls.compl ;
    a=cls.a
    } ;

  -- RelVP : RP -> VP -> RCl ;      -- who loves John
  RelVP rp vp =
    let rp_as_adj = rp.poss ** {preferShort=PrefFull;  sf,sm,sn,sp,comp = []} in {
    subj=(adjFormsAdjective rp_as_adj).s ;
    adv=\\a=>[] ;
    verb=vp.verb ;
    dep=vp.dep ;
    compl=vp.compl ;
    a=rp.a
    } ;

  -- : RP ;                      -- which
  IdRP = lin RP (doKotoryjPron "который" (Ag (GSg Neut) P3) Inanimate) ;

  -- Prep -> NP -> RP -> RP ;  -- the mother of whom
  FunRP prep np rp = (prependIP (np.s ! Nom ++ prep.s) rp) ;   -- TODO: This is wrong... RP should be in agreement, but with records it's a bit hard...

}
