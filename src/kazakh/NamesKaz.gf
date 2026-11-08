concrete NamesKaz of Names = CatKaz ** open Prelude, ResKaz in {
  oper nameNP : Str -> NP = \s -> lin NP {s=\\_ => s; a={p=P3;n=Sg}} ;

  lin
    UseLN ln=nameNP ln.s;
    InLN ln={s=ln.s++"да"};
    AdjLN ap ln={s=ap.s++ln.s};
    GivenName gn=nameNP gn.s;
    MaleSurname sn=nameNP sn.s;
}
