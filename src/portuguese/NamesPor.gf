concrete NamesPor of Names = CatPor ** open ResPor, CommonRomance, Prelude in {

lin GivenName = \n -> pn2np n ;
lin MaleSurname = \n -> pn2np {s = n.s ! Masc; g = Masc} ;
lin FemaleSurname = \n -> pn2np {s = n.s ! Fem; g = Fem} ;
lin PlSurname = \n -> heavyNPpol False {
       s = \\c => prepCase c ++ n.pl ;
       a = agrP3 Masc Pl
    } ;
lin FullName gn sn = pn2np {
       s = gn.s ++ sn.s ! gn.g ;
       g = gn.g
    } ;

lin PlainLN n = heavyNP {
      s = \\c => n.s; 
      a = {g = n.g ; n = n.num ; p = P3}
      } ;


lin UseLN n = heavyNP {
      s = \\c => case n.art of {
        UseArt => case n.g of {
          Fem => case n.num of {
            Sg => "la" ++ n.s;
            Pl => "las" ++ n.s} ;
          Masc => case n.num of {
            Sg => "el" ++ n.s;
            Pl => "los" ++ n.s
            }
            } ;
        NoArt => n.s
        } ;
      a = {g = n.g ; n = n.num ; p = P3}
      } ;


lin InLN n = {
      s = case n.art of {
        UseArt => case n.g of {
          Fem => case n.num of {
            Sg => "la" ++ n.s;
            Pl => "las" ++ n.s} ;
          Masc => case n.num of {
            Sg => "el" ++ n.s;
            Pl => "los" ++ n.s
             }
            } ;
        NoArt => n.s
        } ;
  } ;


lin AdjLN ap n = n ** {
      s = preOrPost ap.isPre (ap.s ! AF n.g n.num) n.s ;
    } ;

}
