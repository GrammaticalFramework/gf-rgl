concrete NamesGer of Names = CatGer ** open ResGer in {

lin GivenName gn = {
      s = \\_,c => gn.s ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

lin MaleSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Sg ;
      w = WLight ;  
      rc, ext = []
      } ;
      
lin FemaleSurname sn = {
      s = \\_,c => sn.s ! Female ! c ;
      a = agrgP3 Fem Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

lin PlSurname sn = {
      s = \\_,c => sn.s ! Male ! c ;
      a = agrgP3 Masc Pl ;
      w = WLight ;  
      rc, ext = []
      } ;

lin FullName gn sn = {
      s = \\_,c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
      a = agrgP3 (sex2gender gn.g) Sg ;
      w = WLight ;  
      rc, ext = []
      } ;

}
