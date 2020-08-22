concrete RelativeMay of Relative = CatMay ** open
  ResMay, Prelude in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = {
    subj = rp.s ;
    pred = \\per => vp.s ! Root ;
    } ;

  -- : RP -> ClSlash -> RCl ; -- who I went with
  RelSlash rp cls = {
    subj = rp.s            -- yang
        ++ cls.subj ;      -- saya
    pred = \\per,pol =>
      let object : Str = case cls.c2.prepType of {
            OtherPrep
              => cls.c2.obj ! per ; -- depends on the head, not known yet
            _ => [] -- if the preposition is dir.obj or empty, no obj. pronoun
            } ;
      in cls.pred ! Root ! pol   -- ikut sama
      ++ object                  -- dengan+nya
    } ;

  -- : RP ;
  IdRP = {s = "yang"} ;

  -- Mintz page 49: Saya jumpa orang /yang kaki+nya/ patah.
  --               'I met a man /whose foot/ was broken.'
  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  -- FunRP prep np rp = {} ;

}
