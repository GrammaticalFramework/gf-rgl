--# -path=.:../romance:../common:../abstract

resource MakeStructuralIta = open CatIta, ParadigmsIta, MorphoIta, Prelude in {

oper
  mkConj : Str -> Str -> ParadigmsIta.Number -> Conj = \x,y,n ->
    {s1 = x ; s2 = y ; n = n ; lock_Conj = <>} ;
  mkSubj : Str -> Subj = \x ->
    {s = x ; m = Indic ; lock_Subj = <>} ;
  mkSubjSubj : Str -> Subj = \x ->
    {s = x ; m = Conjunct ; lock_Subj = <>} ;

  mkIQuant : Str -> IQuant = \s ->
    {s = \\_,_,c => prepCase c ++ s ; lock_IQuant = <>} ;

  mkPredet = overload {
    mkPredet : A -> Predet = \adj -> lin Predet {
        s = \\a,c => prepCase c ++ adj.s ! genNum2Aform a.g a.n ;
        c = Nom ;
        a = PNoAg
        } ;
    mkPredet : Str -> Str -> Prep -> Bool -> Predet = \m,f,c,p -> lin Predet {
      s = \\g,k => prepCase k ++ case g.g of {Masc => m ; Fem => f} ;  ---- number?
      c = c.c ;
      a = if_then_else PAgr p (PAg Sg) PNoAg ---- e,g, "chacun de"; other possibilities?
      } ;
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
    mkQuant : Str -> Str -> Str -> Str -> Quant = \tutto,tutta,tutti,tutte ->
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
        spn= \\c => prepCase c ++ tutto ;
        s2 = [] ;
        isNeg = False
      } ;
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


}
