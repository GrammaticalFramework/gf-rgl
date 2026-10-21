--# -path=.:../abstract:../common:../hindustani

concrete NamesHin of Names = CatHin ** open ResHin, CommonHindustani in {

  lin
    GivenName name = {s = \\_ => name.s ; a = agrP3 Masc Sg} ;
    MaleSurname name = {s = \\_ => name.s ; a = agrP3 Masc Sg} ;
    FemaleSurname name = {s = \\_ => name.s ; a = agrP3 Fem Sg} ;
    PlSurname name = {s = \\_ => name.s ; a = agrP3 Masc Pl} ;

    FullName given surname = {
      s = \\_ => given.s ++ surname.s ;
      a = agrP3 Masc Sg
      } ;

    UseLN name = {s = \\_ => name.s ; a = agrP3 Masc Sg} ;
    PlainLN name = {s = \\_ => name.s ; a = agrP3 Masc Sg} ;
    InLN name = {s = \\_ => name.s ++ "में"} ;
    AdjLN ap name = {s = ap.s ! Sg ! Masc ! Dir ! Posit ++ name.s} ;
}
