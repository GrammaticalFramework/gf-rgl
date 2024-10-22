concrete NamesIna of Names = CatIna ** open Prelude, ResIna in {

lin GivenName gn = {
      s = \\_ => gn.s; 
      a = agrP3 Sg;
      isPronoun = False
      } ;
lin MaleSurname = \sn -> {
      s = \\_ => sn.s ! Male;
      a = agrP3 Sg;
      isPronoun = False
    } ;
lin FemaleSurname = \sn -> {
      s = \\_ => sn.s ! Female;
      a = agrP3 Sg;
      isPronoun = False
    } ;
lin PlSurname = \sn -> {
      s = \\_ => sn.pl;
      a = agrP3 Pl;
      isPronoun = False
    } ;
lin FullName gn sn = {
      s = \\_ => gn.s ++ sn.s ! gn.g ;
      a = agrP3 Pl;
      isPronoun = False
    } ;

lin UseLN ln = {
      s = \\c => artLN ln c ; 
      a = agrP3 ln.n;
      isPronoun = False
    } ;
lin PlainLN ln = {
      s = \\_ => ln.s; 
      a = agrP3 ln.n;
      isPronoun = False
    } ;

lin InLN ln = {
      s = "in" ++ artLN ln Acc ;
    } ;

lin AdjLN ap ln = ln ** {
      s = preOrPost ap.isPre (ap.s ! agrP3 ln.n) ln.s ;
    } ;

oper
  artLN : LN -> Case -> Str = \ln,c ->
     case ln.art of {
       True  => case c of {
                  Dat => "al";
                  Gen | Abl => "del";
                  _ => "le"
                } ++
                ln.s ;
       False => ln.s
     } ;

}
