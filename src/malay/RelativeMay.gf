concrete RelativeMay of Relative = CatMay ** open
  ResMay, Prelude, (NS=NounMay), (SS=StructuralMay) in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = {
    subj = rp.s ;
    pred = vp.s ! Root ; -- TODO
    } ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  -- RelSlash rp cls =

  -- : RP ;
  IdRP = {s = "yang"} ;

  -- Mintz page 49: Saya jumpa orang /yang kaki+nya/ patah.
  --               'I met a man /whose foot/ was broken.'
  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  -- FunRP prep np rp = {} ;


}
