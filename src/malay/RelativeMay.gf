concrete RelativeMay of Relative = CatMay ** open
  ResMay, Prelude in {


lin
  -- : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = cl ** {
  --   subj = cl.subj ;
  --   pred = cl.pred ;
  -- };

  -- : RP -> VP -> RCl ;
  RelVP rp vp = {
    subj = rp.s ;
    pred = \\per,pol => vp.s ! Active ! pol;
    } ;

  -- : RP -> ClSlash -> RCl ; -- who I went with
  RelSlash rp cls = {
    subj = rp.s            -- yang
        ++ cls.subj ;      -- aku
    pred = \\per,pol =>
      let object : Str = case cls.c2.prepType of {
            OtherPrep
              => cls.c2.obj ! per ; -- depends on the head, not known yet
            _ => [] -- if the preposition is dir.obj or empty, no obj. pronoun
            } ;
      in cls.pred ! Active ! pol   -- ikut sama
      ++ object                  -- dengan+nya
    } ;

  -- : RP ;
  IdRP = {s = "yang"} ;

  -- Mintz page 49: aku jumpa orang /yang kaki+nya/ patah.
  --               'I met a man /whose foot/ was broken.'
  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  -- FunRP prep np rp = {} ;

}
