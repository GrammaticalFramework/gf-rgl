concrete RelativeMay of Relative = CatMay ** open
  ResMay, Prelude in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = {
    subj = rp.s ;
    pred = \\_ => vp.s ! Root ;
    } ;

  -- : RP -> ClSlash -> RCl ; -- who I went with
  RelSlash rp cls = {
    subj = rp.s            -- yang
        ++ cls.subj ;      -- saya
    pred =
      \\p => cls.pred ! Root -- ikut sama
          ++ cls.c2.obj ! p  -- dengan+nya (depends on the head, not known yet)
    } ;

  -- : RP ;
  IdRP = {s = "yang"} ;

  -- Mintz page 49: Saya jumpa orang /yang kaki+nya/ patah.
  --               'I met a man /whose foot/ was broken.'
  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  -- FunRP prep np rp = {} ;

}
