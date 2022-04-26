--# -path=.:../abstract:../common

concrete MarkupFin of Markup = CatFin, MarkHTMLX ** open ResFin in {

lin
  MarkupCN   m cn  = cn ** {s = \\nf   => appMark m (linCN nf cn)} ;
  MarkupNP   m np  = np ** {s = \\c    => appMark m (np.s ! c)} ;
  MarkupAP   m ap  = ap ** {s = \\b,nf => appMark m (ap.s ! b ! nf)} ;
  MarkupAdv  m adv =       {s =          appMark m adv.s} ;
  MarkupS    m s   =       {s =          appMark m s.s} ;
  MarkupUtt  m utt =       {s =          appMark m utt.s} ;
  MarkupPhr  m phr =       {s =          appMark m phr.s} ;
  MarkupText m txt =       {s =          appMark m txt.s} ;

}
