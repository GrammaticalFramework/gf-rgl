--# -path=.:../abstract:../common:../prelude

concrete NamesTel of Names = CatTel ** open ResTel in {
  lin
    GivenName name = {
      s = \\c => name.s ;
      a = agrP3 Masc Sg
      } ;

    MaleSurname name = {
      s = \\c => name.s ;
      a = agrP3 Masc Sg
      } ;

    FemaleSurname name = {
      s = \\c => name.s ;
      a = agrP3 Fem Sg
      } ;

    FullName gn sn = {
      s = \\c => gn.s ++ sn.s ;
      a = agrP3 Masc Sg
      } ;

    UseLN name = {
      s = \\c => name.s ;
      a = agrP3 Neutr Sg
      } ;

    InLN name = {s = name.s ++ "లో"} ;
}
