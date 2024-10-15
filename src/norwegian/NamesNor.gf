concrete NamesNor of Noun = CatNor ** NamesScand - [AdjLN] with
  (ResScand = ResNor) ** {

lin AdjLN ap ln = ln ** {
      s = \\c => preOrPost ap.isPre 
                   (ap.s ! agrAdj (gennum ln.g ln.n) (DDef Def))
                   (ln.s ! c) ;
      } ;

}
