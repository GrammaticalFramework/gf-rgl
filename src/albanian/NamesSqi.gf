concrete NamesSqi of Names = CatSqi ** open ResSqi in {
lin
  GivenName n = {s=\\_=>n.s; a={gn=GSg Masc;p=P3}} ;
  MaleSurname n = {s=\\_=>n.s; a={gn=GSg Masc;p=P3}} ;
  FemaleSurname n = {s=\\_=>n.s; a={gn=GSg Fem;p=P3}} ;
  PlSurname n = {s=\\_=>n.s; a={gn=GPl;p=P3}} ;
  FullName n s = {s=\\_=>n.s++s.s; a={gn=GSg Masc;p=P3}} ;
  UseLN n = {s=\\_=>n.s; a={gn=GSg Masc;p=P3}} ;
  PlainLN n = {s=\\_=>n.s; a={gn=GSg Masc;p=P3}} ;
  InLN n = {s="në"++n.s} ;
  AdjLN a n = {s=n.s++a.s!Def!Nom!Masc!Sg} ;
}
