concrete NamesFre of Names = CatFre ** open Prelude, ResFre, CommonRomance, PhonoFre in {

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
      s = \\c => prepCase c ++ n.s;
      a = {g = n.g ; n = n.num ; p = P3}
      } ;

lin UseLN n = heavyNP {
      s = \\c => case n.art of {
        AlwaysArt | UseArt => artDef True n.g n.num c ++ n.s ;
        _      => n.s
        } ;
      a = {g = n.g ; n = n.num ; p = P3}
  } ;


lin InLN n = {
      s = let p : {s : Str; c:Prepos} =
                case n.onPrep of {
                  True  => {s="en"; c=PNul} ;
                  False => {s="";   c=P_a}
                }
          in p.s ++ case n.art of {
                      AlwaysArt => artDef True n.g n.num (CPrep p.c) ++ n.s;
                      _         => prepCase (CPrep p.c) ++ n.s
                    } ;
  } ;


lin AdjLN ap n = n ** {
      s = preOrPost ap.isPre (ap.s ! AF n.g n.num) n.s ;
    } ;    

}
