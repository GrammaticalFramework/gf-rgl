concrete NamesSwa of Names = CatSwa ** open ResSwa, Prelude in {
  lin
    GivenName n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Sg P3} ;
    MaleSurname n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Sg P3} ;
    FemaleSurname n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Sg P3} ;
    PlSurname n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Pl P3} ;
    FullName g s = {s = \\c => g.s ! npcase2case c ++ s.s ! npcase2case c ; a = Ag g.g Sg P3} ;
    UseLN n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Sg P3} ;
    PlainLN n = {s = \\c => n.s ! npcase2case c ; a = Ag n.g Sg P3} ;
    InLN n = {s = "katika" ++ n.s ! Nom} ;
    AdjLN ap n = {s = \\c => n.s ! c ++ ap.s ! n.g ! Sg ; g = n.g} ;
}
