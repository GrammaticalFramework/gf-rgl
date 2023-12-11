--# -path=.:../romance:../common:../abstract

resource MakeStructuralSpa = open CatSpa, (P = ParadigmsSpa), MorphoSpa, Prelude in {

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


  mkQuant = overload {
    -- Does not inflect for number or gender
    mkQuant : Str -> Quant = \s ->
      let
        questo : Number => Gender => Case => Str = \\n,g,c => prepCase c ++ s ;
      in lin Quant {
        s = \\b => questo ;
        sp = questo ;
        spn= \\c => prepCase c ++ s ;
        s2 = [] ;
        isNeg = False
      } ;
    -- Inflects for number and gender
    mkQuant : Str -> Str -> Str -> Str -> Str -> Quant = \tutto,tutta,tutti,tutte,esto ->
      let
        questo : Number => Gender => Case => Str = table {
          Sg => table {
            Masc => \\c => prepCase c ++ tutto ;
            Fem  => \\c => prepCase c ++ tutta
          } ;
          Pl => table {
            Masc => \\c => prepCase c ++ tutti ;
            Fem  => \\c => prepCase c ++ tutte
          }
        }
      in lin Quant {
        s = \\b => questo ;
        sp = questo ;
        spn= \\c => prepCase c ++ "esto" ;
        s2 = [] ;
        isNeg = False
      } ;
  } ;

  mkDet = overload {
    -- Does not inflect for number
    mkDet : Str -> Number -> Det = \piu,n -> lin Det {
      s,sp = \\_,c => prepCase c ++ piu ;
      spn = \\c => prepCase c ++ piu ;
      n = n ;
      s2 = \\g => [] ;
      isNeg = False
    } ;
    -- Inflects for number
    mkDet : Str -> Str -> Number -> Det = \alcuni,alcune,n -> lin Det {
      s,sp = \\g,c => prepCase c ++ genForms alcuni alcune ! g ;
      spn = \\c => prepCase c ++ alcuni ;
      n = n ;
      s2 = \\g => [] ;
      isNeg = False
    } ;
  } ;

}
