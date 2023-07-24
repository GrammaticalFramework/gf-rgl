abstract Names = Cat ** {

fun GivenName : GN -> NP ;
    MaleSurname   : SN -> NP ;
    FemaleSurname : SN -> NP ;
    PlSurname     : SN -> NP ;
    FullName  : GN -> SN -> NP ;

fun UseLN : LN -> NP ;
    PlainLN : LN -> NP ;
    InLN : LN -> Adv ;
    AdjLN : AP -> LN -> LN ;

}
