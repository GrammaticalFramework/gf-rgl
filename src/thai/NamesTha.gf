concrete NamesTha of Names = CatTha ** open ResTha, Prelude in {

  lin
    GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ;
    FullName gn sn = mkNP (thbind gn.s sn.s) ;

    UseLN, PlainLN = \ln -> ln ;
    AdjLN ap ln = mkNP (thbind ln.s ap.s) ;
    InLN ln = ss (thbind "ใน" ln.s) ;
}
