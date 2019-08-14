concrete RelativeSom of Relative = CatSom ** open
  ResSom, Prelude, (NS=NounSom), (SS=StructuralSom) in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = {s = cl.s ! Subord} ;

  -- : RP -> VP -> RCl ;
  {- NB. this works because vfSubord only puts different forms from vfStatement
    in Pres,Simul,Pos. RelVP needs a third set of forms in Abs,Pres,Simul,Pos,
    called "reduced present general" (Sayeed p. 95-96 + ch 8).
    These forms are found in VRel in VP, and aren't chosen by predVP, so we put
    them in manually in RelVP.
  -}
  RelVP rp vp = {s = \\g,c,t,a,p =>
    let cls = predVPSlash impersNP vp ;
        rclSubord = mergeRCl (cls.s ! True) ;
        rclStatement = mergeRCl (cls.s ! False) ;
    in rp.s ++ case <g,c,t,a,p> of {
        <g,Abs,Pres,Simul,Pos> => linVP (VRel g) vp ; -- reduced present only in absolutive
        <_,Nom,Pres,Simul,Pos> => rclStatement.s ! t ! a ! p ; -- the usual forms, not subordinate
        _ => rclSubord.s ! t ! a ! p } -- the rest is Subord because of negation.
    } ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls =
    let rcl = mergeSTM True (cls.s ! True) -- in subordinate clause, STM is not included but subject pronoun is
     in rcl ** {s = \\g,c,t,a,p => rp.s ++ rcl.s ! t ! a ! p} ;


  -- : RP ;
  IdRP = {s = ""} ; -- no overt relative pronoun "that, which". For "what" e.g. "tell me what you saw", use waxa. (Nilsson p. 107)

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

}
