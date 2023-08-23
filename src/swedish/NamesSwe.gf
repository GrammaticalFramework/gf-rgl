concrete NamesSwe of Names = CatSwe ** open CommonScand, ResSwe, Prelude in {

lin GivenName = \pn -> {
      s = \\c => pn.s ! caseNP c ; 
      a = agrP3 Utr Sg ;
      isPron = False
      } ;
lin MaleSurname = \pn -> {
      s = \\c => pn.s ! Male ! caseNP c ; 
      a = agrP3 Utr Sg ;
      isPron = False
      } ;
lin FemaleSurname = \pn -> {
      s = \\c => pn.s ! Female ! caseNP c ; 
      a = agrP3 Utr Sg ;
      isPron = False
      } ;
lin PlSurname = \pn -> {
      s = \\c => pn.pl ! caseNP c ; 
      a = agrP3 Utr Pl ;
      isPron = False
      } ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! caseNP c ;
      a = agrP3 Utr Sg ;
      isPron = False
      } ;

    UseLN, PlainLN = \n -> {
      s = \\c => n.s ! caseNP c ; 
      a = agrP3 n.g n.n ;
      isPron = False
      } ;

    InLN n = {s = "i" ++ n.s ! caseNP accusative} ;

}
