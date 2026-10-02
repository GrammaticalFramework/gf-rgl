concrete NamesPol of Names = CatPol ** open ResPol in {

lin GivenName, MaleSurname, FemaleSurname = \n -> n ;
lin FullName gn sn = {
       nom = gn.nom ++ sn.nom ;
       voc = gn.nom ++ sn.voc ;
       dep = \\c => gn.nom ++ sn.dep ! c ;
       gn = gn.gn ;
       p  = gn.p
    } ;

lin UseLN n = n;

lin InLN n = {s = "w" ++ n.dep ! LocPrep} ;

lin AdjLN ap n = {
       nom = ap.s ! AF n.gn Nom ++ n.nom ;
       voc = ap.s ! AF n.gn VocP ++ n.voc ;
       dep = \\cc => ap.s ! AF n.gn (extract_case ! cc) ++ n.dep ! cc ;
       gn = n.gn ;
       p = n.p
    } ;

}
