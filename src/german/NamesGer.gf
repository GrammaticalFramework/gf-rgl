concrete NamesGer of Names = CatGer ** open ResGer, Prelude in {

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

lin UseLN ln = {
      s = \\c => case ln.hasArt of {
                   True  => artDefContr (gennum ln.g ln.n) c ++ usePrepC c (\k -> ln.s ! Weak ! k) ;
                   False => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k)
                 } ;
      a = agrgP3 ln.g ln.n ;
      w = WLight ;  
      rc, ext = []
      } ;

lin PlainLN ln = {
      s = \\c => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k) ;
      a = agrgP3 ln.g ln.n ;
      w = WLight ;  
      rc, ext = []
      } ;

lin InLN ln = {
      s = let c = NPP CInDat
          in case ln.hasArt of {
               True  => artDefContr (gennum ln.g ln.n) c ++ usePrepC c (\k -> ln.s ! Weak ! k) ;
               False => usePrepC c (\k -> ln.s ! adjfCase Strong k ! k)
             } ;
      } ;

lin AdjLN ap ln = ln ** {
      s = \\a,c => 
               preOrPost ap.isPre
                 (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj ln.g a ln.n c ++ ap.ext)
                 (ln.s ! a ! c) ;
      } ;

}
