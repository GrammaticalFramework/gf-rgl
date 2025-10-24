concrete RelativeMkd of Relative = CatMkd ** open Prelude,ResMkd in {
  lin FunRP p np rp = {s = \\g => p.s ++ np.s ! RSubj ++ rp.s ! g} ;
  lin IdRP = {s = table {
                    GSg Masc => "кој" ;
                    GSg Fem => "која" ;
                    GSg Neuter => "кое" ;
                    GPl => "кои"
                  }
             } ;
  lin RelCl cl = {s = \\g => cl.s ! Main} ;
  lin RelSlash rp c = {s = \\g,t,a,p => rp.s ! g ++ c.s} ;
  lin RelVP rp vp = {
        s = \\gn => mkClause (rp.s ! gn) {g=gn; p=P3} vp ! Main
      } ;
}
