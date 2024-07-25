--# -path=.:../romance:../common:../abstract

resource MakeStructuralPor = open CatPor, (P = ParadigmsPor), MorphoPor, Prelude in {

oper 
  mkConj : Str -> Str -> Number -> Conj = \x,y,n -> 
    {s1 = x ; s2 = y ; n = n ; lock_Conj = <>} ;
  mkSubj : Str -> Subj = \x -> 
    {s = x ; m = Indic ; lock_Subj = <>} ;
  mkSubjSubj : Str -> Subj = \x -> 
    {s = x ; m = Conjunct ; lock_Subj = <>} ;

  mkIQuant : Str -> IQuant = \s ->
    {s = \\_,_,c => prepCase c ++ s ; lock_IQuant = <>} ;

  mkPredet : Str -> Str -> Prep -> Bool -> Predet = \m,f,c,p -> lin Predet {
    s = \\g,k => prepCase k ++ case g.g of {Masc => m ; Fem => f} ; 
    c = c.c ; 
    a = if_then_else PAgr p (PAg Sg) PNoAg ---- e,g, "chacun de"; other possibilities?
    } ;

  mkDet = overload {
    -- singular, does not inflect for gender
    mkDet : Str ->  Det = \piu -> lin Det {
      s,sp = \\_,_ => piu ;
      spn = \\_ => piu ;
      n = Sg ;
      s2 = \\g => [] ;
      isNeg = False
    } ;
    mkDet : Str -> Number -> Det = \piu,n -> lin Det {
      s,sp = \\_,_ => piu ;
      spn = \\_ => piu ;
      n = n ;
      s2 = \\g => [] ;
      isNeg = False
    } ;
    -- Inflects for gender
    mkDet : Str -> Str -> Number -> Det = \alcuni,alcune,n -> lin Det {
      s,sp = table {
        Masc => \\_ => alcuni ;
        Fem  => \\_ => alcune
      } ;
      spn = \\_ => alcuni ;
      n = n ;
      s2 = \\g => [] ;
      isNeg = False
    } ;
  } ;


} ;
