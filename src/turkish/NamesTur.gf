concrete NamesTur of Names = CatTur ** open ResTur in {

lin GivenName, MaleSurname, FemaleSurname = \n -> {
      s = \\c => n.s ! c;
      h = n.h;
      a = {n = Sg; p = P3}
    } ;
lin PlSurname = \n -> {
      s = \\c => n.s ! c;
      h = n.h;
      a = {n = Pl; p = P3}
    } ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! c;
      h = sn.h;
      a = {n = Sg; p = P3}
    } ;

lin UsePN pn = { 
      s = \\c => pn.s ! c;
      h = pn.h;
      a = {n = pn.n; p = P3}
    } ;

lin UseLN ln = {
      s = ln.s;
      h = ln.h;
      a = {n = ln.n; p = P3}
    } ;

lin PlainLN = UseLN ;

lin InLN ln = {s = ln.s ! Loc} ;

lin AdjLN ap ln = ln ** {
      s = \\c => ap.s ! Sg ! Nom ++ ln.s ! c
    } ;

}
