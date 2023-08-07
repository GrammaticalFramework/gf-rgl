concrete NamesSwe of Names = CatSwe ** open CommonScand, ResSwe, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \pn -> {
      s = \\c => pn.s ! caseNP c ; 
      a = agrP3 pn.g Sg ;
      isPron = False
      } ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! caseNP c ;
      a = agrP3 gn.g Sg ;
      isPron = False
      } ;

    UseLN, PlainLN = \n -> {
      s = \\c => n.s ! caseNP c ; 
      a = agrP3 n.g n.n ;
      isPron = False
      } ;

    InLN n = {s = n.c.s ++ n.s ! caseNP accusative} ;

}
