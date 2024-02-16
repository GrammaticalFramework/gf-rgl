concrete NamesFin of Names = CatFin ** open ResFin, StemFin, Prelude in {

lin GivenName n = {
      s = snoun2np Sg n ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;
lin MaleSurname n = {
      s = snoun2np Sg (n.s ! Male) ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;
lin FemaleSurname n = {
      s = snoun2np Sg (n.s ! Female) ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;
lin PlSurname n = {
      s = snoun2np Pl n.pl ;
      a = agrP3 Pl ;
      isPron = False ; isNeg = False
      } ;
lin FullName gn sn = {
      s = snoun2np Sg {s = \\c => gn.s ! Nom ++ (sn.s ! gn.g).s ! c} ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;

lin UseLN, PlainLN = \ln -> {
      s = snoun2np ln.n ln ;
      a = agrP3 ln.n ;
      isPron = False ; isNeg = False
      } ;

lin InLN ln =
   let c = case ln.extCase of {True => Adess ; False => Iness}
   in {s = ln.s ! c} ;

}
