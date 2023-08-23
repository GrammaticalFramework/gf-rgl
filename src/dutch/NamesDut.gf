concrete NamesDut of Names = CatDut ** open Prelude, ResDut in {

lin GivenName = \n -> noMerge ** {s = n.s ; a = agrP3 Sg ; isPron = False} ;
lin MaleSurname = \n -> noMerge ** {s = n.s ! Male ; a = agrP3 Sg ; isPron = False} ;
lin FemaleSurname = \n -> noMerge ** {s = n.s ! Female; a = agrP3 Sg ; isPron = False} ;
lin PlSurname = \n -> noMerge ** {s = n.pl ; a = agrP3 Sg ; isPron = False} ;
lin FullName gn sn =
       noMerge ** {s = \\c => gn.s ! NPNom ++ sn.s ! gn.g ! c ; a = agrP3 Sg ; isPron = False} ;

lin UseLN ln = noMerge ** {
      s = \\c => case ln.hasArt of {
                   True  => "de" ++ ln.s ! Weak ! c ;
                   False => ln.s ! Strong ! c
                 } ;
      a = agrP3 ln.n ;
      isPron = False
    } ;

lin PlainLN ln = noMerge ** {
      s = \\c => ln.s ! Strong ! c ;
      a = agrP3 ln.n ;
      isPron = False
    } ;

lin InLN ln = {
      s = "in" ++ case ln.hasArt of {
                    True  => "de" ++ ln.s ! Weak ! NPAcc ;
                    False => ln.s ! Strong ! NPAcc
                  }
    } ;

lin AdjLN ap ln = ln ** {
      s = \\a,c =>
               let gan : Gender*Adjf*NForm = case ap.isPre of {
                    True  => <Utr,a,NF ln.n Nom> ;
                    False => <Neutr,Strong,NF Sg Nom> } ;
                   af = agrAdj gan.p1 gan.p2 gan.p3 ;
               in preOrPost ap.isPre
                    (ap.s ! agrP3 Sg ! af)
                    (ln.s ! a ! c) ;
      } ;

}
