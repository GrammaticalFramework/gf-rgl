concrete NamesHye of Names = CatHye ** open Prelude, ResHye in {
lin
  GivenName n = {s=\\_=>n.s;a={n=Sg;p=P3}} ;
  MaleSurname n = {s=\\_=>n.s;a={n=Sg;p=P3}} ;
  FemaleSurname n = {s=\\_=>n.s;a={n=Sg;p=P3}} ;
  PlSurname n = {s=\\_=>n.s;a={n=Pl;p=P3}} ;
  FullName n s = {s=\\_=>n.s++s.s;a={n=Sg;p=P3}} ;
  UseLN n = {s=\\_=>n.s;a={n=Sg;p=P3}} ;
  PlainLN n = {s=\\_=>n.s;a={n=Sg;p=P3}} ;
  InLN n = {s=n.s++BIND++"-ում"} ;
  AdjLN a n = {s=a.s!Indef!Nom!Sg++n.s} ;
}
