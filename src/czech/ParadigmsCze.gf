resource ParadigmsCze = open CatCze, ResCze, Prelude in {

oper
  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s ; c = c ; hasPrep = True} ; ---- True if s /= ""
    
  mkConj : Str -> Conj
    = \s -> lin Conj {s1 = [] ; s2 = s} ;

  mkA : Str -> A
    = \s -> lin A (case s of {
      _ + "ý"  => mladyAdjForms s ;
      _ + "í"  => jarniAdjForms s ;
      _ + "ův" => otcuvAdjForms s ;
      _ + "in" => matcinAdjForms s ;
      _ => Predef.error ("no mkA for" ++ s)
      }) ;

  mkV2 = overload {
    mkV2 : VerbForms -> VerbForms ** {c : ComplementCase}
      = \vf -> vf ** {c = {s = [] ; c = Acc ; hasPrep = False}} ;
    mkV2 : VerbForms -> Case -> VerbForms ** {c : ComplementCase}
      = \vf,c -> vf ** {c = {s = [] ; c = c ; hasPrep = False}} ;
    mkV2 : VerbForms -> ComplementCase -> VerbForms ** {c : ComplementCase}
      = \vf,c -> vf ** {c = c} ;
    } ;
    

}
