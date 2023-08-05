concrete NamesSpa of Names = CatSpa ** open Prelude, ResSpa, CommonRomance in {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> pn2np n ;
lin FullName gn sn = pn2np {
       s = gn.s ++ sn.s ;
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
      s = n.p.s ++ case n.art of {
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
