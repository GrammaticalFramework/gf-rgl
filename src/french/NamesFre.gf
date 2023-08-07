concrete NamesFre of Names = CatFre ** open Prelude, ResFre, CommonRomance, PhonoFre in {

lin GivenName, MaleSurname, FemaleSurname = \n -> pn2np n ;
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
        AlwaysArt | UseArt => artDef True n.g n.num c ++ n.s ;
        _      => n.s
        } ;
      a = {g = n.g ; n = n.num ; p = P3}
  } ;


lin InLN n = {
      s = n.p.s ++ case n.art of {
        AlwaysArt => artDef True n.g n.num n.p.c ++ n.s;
        _         => prepCase n.p.c ++ n.s
        } ;
  } ;


lin AdjLN ap n = n ** {
      s = preOrPost ap.isPre (ap.s ! AF n.g n.num) n.s ;
    } ;    

}
