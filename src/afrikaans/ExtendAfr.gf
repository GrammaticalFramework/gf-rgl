concrete ExtendAfr of Extend =
  CatAfr ** ExtendFunctor
  with
    (Grammar = GrammarAfr) **

  open
    ResAfr in {

lin GivenName, MaleSurname, FemaleSurname = \n -> n ;
lin FullName gn sn = {
       s = \\c => gn.s ! NPNom ++ sn.s ! c ; 
    } ;

}
