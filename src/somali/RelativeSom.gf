concrete RelativeSom of Relative = CatSom ** open
  ResSom, Prelude, (NS=NounSom), (SS=StructuralSom) in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = {s = cl.s ! Subord} ;

  -- : RP -> VP -> RCl ;
  {- NB. this works because vfSubord only puts different forms from vfStatement
    in Pres,Simul,Pos. RelVP needs a third set of forms in Abs,Pres,Simul,Pos,
    called "reduced present general" (Saeed p. 95-96 + ch 8).
    These forms are found in VRel in VP, and aren't chosen by predVP, so we put
    them in manually in RelVP.
  -}
  RelVP rp vp = {s = \\gn,c,t,a,p =>
    let cls = predVP impersNP vp ;
        rclSubord = cl2rcl cls ;
        rclStatement = cl2rclNom cls ;
    in rp.s ++ case <gn,c,t,a,p> of {
        <g,Abs,Pres,Simul,Pos> => linVP (VRel g) Subord vp ; -- reduced present only in absolutive
        <_,Abs,Pres,Simul,Neg> => linVP VRelNeg Subord vp ; -- special form for have and be
        <_,Nom,Pres,Simul,Pos> => rclStatement.s ! t ! a ! p ; -- as nominative, use the usual forms, not subordinate
        _ => rclSubord.s ! t ! a ! p } -- the rest is Subord because of negation.
    } ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls =
    let rcl = cl2relslash cls -- in subordinate clause, STM is not included but subject pronoun is
     in rcl ** {s = \\g,c,t,a,p => rp.s ++ rcl.s ! t ! a ! p} ;


  -- : RP ;
  IdRP = {s = ""} ; -- no overt relative pronoun "that, which". For "what" e.g. "tell me what you saw", use waxa. (Nilsson p. 107)

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

oper

  -- RelVP: subject pronoun is never included
  cl2rcl : ClSlash -> Clause =
    let hasSubjPron : Bool = False ;
        hasSTM : Bool = False ;
        isRel : Bool = True ;
     in mkClause Subord isRel hasSubjPron hasSTM ;

  -- No subject pronoun, no STM, but use verb forms from Statement
  cl2rclNom : ClSlash -> Clause = \cls ->
    let hasSubjPron : Bool = False ;
        hasSTM : Bool = False ;
        isRel : Bool = True ;
      in mkClause Statement isRel hasSubjPron hasSTM cls ;

  -- RelSlash: subject pronoun is included if it's not 3rd person
  -- TODO check this rule with more example sentences
  cl2relslash : ClSlash -> Clause =
    let hasSubjPron : Bool = True ;
        hasSTM : Bool = False ;
        isRel : Bool = True ;
     in mkClause Subord isRel hasSubjPron hasSTM ;

}
