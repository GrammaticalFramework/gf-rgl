concrete NamesRus of Names = CatRus ** open ResRus, Prelude in {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \pn ->
      { s=\\cas => (nounFormsNoun pn).s ! Sg ! cas ;
        pron=False;
        a=Ag (gennum pn.g Sg) P3
      } ;   -- Does NP need animacy?

lin FullName gn sn =
      { s= table {
             Nom => gn.snom ++ sn.snom ;
             Gen => gn.snom ++ sn.sgen ;
             Dat => gn.snom ++ sn.sdat ;
             Acc => gn.snom ++ sn.sacc ;
             Ins => gn.snom ++ sn.sins ;
             Pre => gn.snom ++ sn.sprep ;
             Loc => gn.snom ++ sn.sloc ;
             Ptv => gn.snom ++ sn.sptv ;
             VocRus => gn.snom ++ sn.svoc
           } ;
        pron=False;
        a=Ag (gennum gn.g Sg) P3
      } ;

  UseLN pn = {
    s=\\cas => (nounFormsNoun pn).s ! Sg ! cas ;
    pron=False;
    a=Ag (gennum pn.g Sg) P3
    } ;   -- Does NP need animacy?

}
