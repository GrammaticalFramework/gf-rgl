--# -path=.:../common:../abstract

resource MakeStructuralFin = open CatFin, ParadigmsFin, MorphoFin, Prelude in {

oper 
  mkConj : Str -> Str -> ParadigmsFin.Number -> Conj = \x,y,n -> 
    {s1 = x ; s2 = y ; n = n ; lock_Conj = <>} ;
  mkSubj : Str -> Subj = \x -> 
    {s = x ; lock_Subj = <>} ;
  mkIQuant : Str -> IQuant = \s ->
    {s = \\n,c => s ; lock_IQuant = <>} ; ----

  mkIDet : Bool -> Str -> N -> Number -> IDet = \isNum,pref,s,n ->
    lin IDet {s = \\c => pref ++ s.s ! NCase n c ; n = n ; isNum = isNum} ;

  mkDet : N -> Number -> Det = \s,n ->
    lin Det (MorphoFin.mkDet n s) ;
  partDet : N -> Number -> Det = \s,n ->
    lin Det (MorphoFin.partDet False n s) ;

}
