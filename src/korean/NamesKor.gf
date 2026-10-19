concrete NamesKor of Names = CatKor ** open Prelude, ResKor in {

lin GivenName, MaleSurname, FemaleSurname = \n -> n ;
lin FullName gn sn = {
       s = \\nf => gn.s ! nf ++ sn.s ! nf ;
       p = gn.p
    } ;

  UseLN pn = pn ;
  InLN pn = {s = glue (pn.s ! Bare) "에서"} ;
  AdjLN ap pn = pn ** {s = \\nf => ap.compar ++ ap.s ! VAttr Pos ++ pn.s ! nf} ;

}
