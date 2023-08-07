concrete NamesGer of Names = CatGer ** open ResGer in {

lin GivenName gn = {
      s = \\c => usePrepC c (\k -> gn.s ! k) ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

lin MaleSurname sn = {
      s = \\c => usePrepC c (\k -> sn.s ! Male ! k) ;
      a = agrgP3 Masc Sg ;
      w = WLight ;  
      rc, ext = []
      } ;
      
lin FemaleSurname sn = {
      s = \\c => usePrepC c (\k -> sn.s ! Female ! k) ;
      a = agrgP3 Fem Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

lin PlSurname sn = {
      s = \\c => usePrepC c (\k -> sn.s ! Male ! k) ;
      a = agrgP3 Masc Pl ;
      w = WLight ;  
      rc, ext = []
      } ;

lin FullName gn sn = {
      s = \\c => usePrepC c (\k -> gn.s ! Nom ++ sn.s ! gn.g ! k) ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

lin UseLN pn = {
      s = \\c => usePrepC c (\k -> pn.s ! k) ;
      a = agrgP3 pn.g pn.n ;
      w = WLight ;  
      rc, ext = []
      } ;

}
