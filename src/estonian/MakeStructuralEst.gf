resource MakeStructuralEst = open CatEst, ParadigmsEst, MorphoEst, Prelude in {

oper
  mkConj : Str -> Str -> ParadigmsEst.Number -> Conj = \x,y,n -> lin Conj {
    s1 = x ;
    s2 = y ;
    n = n
    } ;
  mkSubj : Str -> Subj = \x -> lin Subj {s = x} ;
  mkIQuant : Str -> IQuant = \s -> lin IQuant {s = \\n,c => s} ;

}
