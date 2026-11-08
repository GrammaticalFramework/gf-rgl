--# -path=.:../abstract:../common:../prelude

concrete NamesCze of Names = CatCze ** open ResCze, Prelude in {

lin
  GivenName name =
    npForms (\\_ => name.s) (\\_ => name.s) ** {
      clit = \\_ => name.s ; a = Ag (Masc Anim) Sg P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  MaleSurname name =
    npForms (\\_ => name.s) (\\_ => name.s) ** {
      clit = \\_ => name.s ; a = Ag (Masc Anim) Sg P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  FemaleSurname name =
    npForms (\\_ => name.s) (\\_ => name.s) ** {
      clit = \\_ => name.s ; a = Ag Fem Sg P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  PlSurname name =
    npForms (\\_ => name.s) (\\_ => name.s) ** {
      clit = \\_ => name.s ; a = Ag (Masc Anim) Pl P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  FullName name surname =
    npForms (\\_ => name.s ++ surname.s) (\\_ => name.s ++ surname.s) ** {
      clit = \\_ => name.s ++ surname.s ;
      a = Ag (Masc Anim) Sg P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  UseLN name =
    npForms (\\_ => name.s) (\\_ => name.s) ** {
      clit = \\_ => name.s ; a = Ag Neutr Sg P3 ;
      hasClit = False ; isDrop = False ; isPron = False
      } ;
  PlainLN = UseLN ;
  InLN name = {s = "v" ++ name.s} ;
  AdjLN ap name = {s = ap.s ! Neutr ! Sg ! Nom ++ name.s} ;

}
