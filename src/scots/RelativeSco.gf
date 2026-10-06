concrete RelativeSco of Relative = RelativeEng - [RelVP] ** open ResSco in {

lin RelVP rp vp = {
      s = \\t,ant,b,ag =>
        let agr = case rp.a of {RNoAg => ag ; RAg a => a} ;
            cl = mkClause (rp.s ! RC (fromAgr agr).g npNom) agr vp
        in cl.s ! t ! ant ! b ! oDir ;
      c = npNom
      } ;

}
