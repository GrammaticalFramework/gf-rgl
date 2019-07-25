concrete RelativeSom of Relative = CatSom ** open
  ResSom, Prelude, (NS=NounSom), (SS=StructuralSom) in {


lin
  --  : Cl -> RCl ;            -- such that John loves her
  -- RelCl cl = {s = cl.s ! Subord} ;

{-
  -- Sayeed p. 95-96 + ch 8
Reduced present general in relative clauses;  as absolutive
1/2SG/3SG M/2PL/3PL sugá -- same as imperative (TODO check if for all conjugations)
3 SG F sugtá -- not yet in the grammar
1PL sugná -- not yet in the grammar

(18) (a) nimánka buugágga keená men-the books-the bring
'the men who bring the books'
(b) buugágga nimanku keenàan
books-the men-the bring 'the books which the men bring'
-}
  -- : RP -> VP -> RCl ;
  RelVP rp vp = RelSlash rp (predVPSlash emptyNP vp) ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls = {
    s = \\t,a,p => rp.s ++ cls.s ! True ! t ! a ! p
    } ;


  -- : RP ;
  IdRP = {s = "waxa"} ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

}
