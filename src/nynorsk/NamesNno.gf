concrete NamesNno of Noun = CatNno ** NamesScand - [AdjLN] with
  (ResScand = ResNno) ** {

lin AdjLN ap ln = ln ** {
      s = \\c => preOrPost ap.isPre 
                   (ap.s ! agrAdj (gennum ln.g ln.n) (DDef Def))
                   (ln.s ! c) ;
      } ;

}
