concrete RelativeSom of Relative = CatSom ** open
  ResSom, Prelude, (NS=NounSom), (SS=StructuralSom) in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = {s = cl.s ! Subord} ;

{-
  -- Sayeed p. 95-96 + ch 8
Reduced present general in relative clauses;  as absolutive
1/2SG/3SG M/2PL/3PL sugá (VRel Masc)
3 SG F sugtá (VRel Fem)
1PL sugná -- not yet in the grammar

(18) (a) nimánka buugágga keená men-the books-the bring
'the men who bring the books'
(b) buugágga nimanku keenàan
books-the men-the bring 'the books which the men bring'
-}
  -- : RP -> VP -> RCl ;
  RelVP rp vp = {s = \\g,c,t,a,p =>
    let cls = predVPSlash impersNP vp ;
        rcl = mergeRCl (cls.s ! False) ; -- Other than present tense, just use normal verb forms
    in rp.s ++ case <g,c,t,a,p> of {
        <Fem,Abs,Pres,Simul,Pos> => linVP (VRel Fem) vp ;
        <Masc,Abs,Pres,Simul,Pos> => linVP (VRel Masc) vp ;
        _ => rcl.s ! t ! a ! p }
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
