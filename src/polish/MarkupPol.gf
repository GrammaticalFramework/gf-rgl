--# -path=.:../abstract:../common

concrete MarkupPol of Markup = CatPol, MarkHTMLX ** open ResPol in {

lin
  MarkupCN   m cn  = cn ** {s = \\n,c => appMark m (cn.s ! n ! c)} ;

  -- NP has three alternative surface forms: nom, voc and the dependent
  -- cases; each is marked up separately.
  MarkupNP   m np  = np ** {
    nom = appMark m np.nom ;
    voc = appMark m np.voc ;
    dep = \\c => appMark m (np.dep ! c)
    } ;

  MarkupAP   m ap  = ap ** {s = \\af => appMark m (ap.s ! af)} ;
  MarkupAdv  m adv =       {s =         appMark m adv.s} ;
  MarkupS    m s   =       {s =         appMark m s.s} ;
  MarkupUtt  m utt =       {s =         appMark m utt.s} ;
  MarkupPhr  m phr =       {s =         appMark m phr.s} ;
  MarkupText m txt =       {s =         appMark m txt.s} ;

}
