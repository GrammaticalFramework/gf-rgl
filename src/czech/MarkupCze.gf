--# -path=.:../abstract:../common

concrete MarkupCze of Markup = CatCze, MarkHTMLX ** open ResCze, Prelude in {

lin
  MarkupCN   m cn  = cn ** {s = \\n,c   => appMark m (cn.s ! n ! c)} ;

  -- s, clit and prep are alternative surface forms, so each is marked;
  -- but clit ! Nom is the form selected by ProDrop, empty for every pronoun,
  -- and marking it up would leave the tags around nothing
  MarkupNP   m np  = np ** {
    s    = \\c => appMark m (np.s ! c) ;
    clit = \\c => case c of {
      Nom => np.clit ! Nom ;
      _   => appMark m (np.clit ! c)
      } ;
    prep = \\c => appMark m (np.prep ! c)
    } ;

  MarkupAP   m ap  = ap ** {
    s = \\g,n,c => appMark m (ap.s ! g ! n ! c) ;
    pred = \\a => appMark m (ap.pred ! a)
    } ;
  MarkupAdv  m adv =       {s =            appMark m adv.s} ;
  -- The complete s and fronted orders each use one wrapper. Later fronting
  -- composes separately marked clitic/body pieces; their wrappers remain
  -- separate even when the pieces become adjacent.
  MarkupS    m s   = s ** {
    s = appMark m s.s ; fronted = appMark m s.fronted ;
    clit = case s.clitPresent of {True => appMark m s.clit ; False => s.clit} ;
    body = appMark m s.body
    } ;
  MarkupUtt  m utt =       {s =            appMark m utt.s} ;
  MarkupPhr  m phr =       {s =            appMark m phr.s} ;
  MarkupText m txt =       {s =            appMark m txt.s} ;

}
