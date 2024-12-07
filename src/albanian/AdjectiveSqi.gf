concrete AdjectiveSqi of Adjective = CatSqi ** open ResSqi, Prelude in {

  lin
    PositA  a = {
      s = \\spec,c,g,n => case a.clit of {
                            True  => link_clitic ! spec ! c ! g ! n ++ a.s ! c ! g ! n ;
                            False => a.s ! c ! g ! n
                          }
      } ;

}
