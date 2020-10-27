--# -path=.:../common:../abstract

resource MakeStructuralEng = open CatEng, ParadigmsEng, ResEng, MorphoEng, Prelude in {

oper 
  mkSubj : Str -> Subj = \x -> 
    lin Subj {s = x} ;
  mkNP : Str -> ParadigmsEng.Number -> NP = \s,n ->
    lin NP (regNP s n) ;
  mkIDet : Str -> ParadigmsEng.Number -> IDet = \s,n ->
    lin IDet {s = s ; n = n} ;
  mkIQuant : Str -> Str -> IQuant = \s,n ->
    lin IQuant {s = table {Sg => s ; Pl => n}} ;
    
  mkDet = overload {
    mkDet : Str -> Number -> Det = \s,n ->
      lin Det (mkDeterminer n s) ;
    mkDet : Str -> Det -> Det = \s,d ->
      d ** {
        s = s ++ d.s ;
	sp = \\g,b,c => s ++ d.sp ! g ! b ! c ;
      } ;
    } ;

}
