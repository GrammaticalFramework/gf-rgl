concrete ExtendAfr of Extend =
  CatAfr ** ExtendFunctor
  with
    (Grammar = GrammarAfr) **

  open
    ResAfr in {

lin GivenName, Surname = \n -> n ;
lin FullName gn sn = {
       s = \\c => gn.s ! NPNom ++ sn.s ! c ; 
    } ;

}
