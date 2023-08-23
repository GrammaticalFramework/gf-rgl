concrete RelativeGer of Relative = CatGer ** open Prelude, ResGer in {

  flags optimize=all_subs ;

  lin

    RelCl cl = {
      s = \\m,t,a,b,_ => "derart" ++ conjThat ++ cl.s ! m ! t ! a ! b ! Sub ;
      c = Nom
      } ;

    RelVP rp vp = {
      s = \\m,t,ant,b,rgn => 
        let
	  gn = case rgn of {
	    RGenNum gf => gf ;
	    RSentence => GSg Neutr
	    } ;
          agr = case rp.a of {
            RNoAg => agrP3 (numGenNum gn) ;
            RAg n p => Ag Neutr n p
            } ;
          cl = mkClause (rp.s ! rgn ! Nom) agr vp
        in
        cl.s ! m ! t ! ant ! b ! Sub ;
      c = Nom
      } ;

    RelSlash rp slash = {
      s = \\m,t,a,p,gn => 
          appPrep slash.c2 (rp.s ! gn) ++ slash.s ! m ! t ! a ! p ! Sub ;
      c = slash.c2.c
      } ;

    FunRP p np rp = {
      s = \\gn,c => np.s ! False ! c ++ appPrep p (rp.s ! gn) ;
      a = RAg (numberAgr np.a) (personAgr np.a)
      } ;

    IdRP = {s = relPron ; a = RNoAg} ;

}
