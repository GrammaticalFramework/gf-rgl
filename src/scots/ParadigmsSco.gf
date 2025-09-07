resource ParadigmsSco = ParadigmsEng - [regV, reg2V, regDuplV, irregV, irreg4V, irregDuplV, mkV, mkV2, mkV3, mkV2V, mkV2Q] ** open Prelude, ResSco, CatSco in {

oper
  regV cry =
    let
      cries = (regN cry).s ! Pl ! Nom ; -- !
      cried : Str = case cries of {
        _ + "es" => init cries + "d" ;
        _ + "ers" => init cries + "ed" ;
        _        => duplFinal cry + "ed"
        } ;
      cryin : Str = case cry of {
        _  + "ee" => cry + "in" ;
        d  + "ie" => d  + "yin" ;
        us + "e"  => us + "in" ;
        ent + "er" => ent + "erin" ;
        _         => duplFinal cry + "in"
        }
    in mk5V cry cries cried cried cryin ;

  reg2V fit fitted =
   let fitt = Predef.tk 2 fitted ;
   in
     if_then_else V (pbool2bool (Predef.eqStr (last fit) (last fitt)))
       (mk5V fit (fit + "s") (fitt + "ed") (fitt + "ed") (fitt + "in"))
       (regV fit) ;

  regDuplV fit =
    case last fit of {
      ("a" | "e" | "i" | "o" | "u" | "y") =>
        Predef.error (["final duplication makes no sense for"] ++ fit) ;
      t =>
       let fitt = fit + t in
       mk5V fit (fit + "s") (fitt + "ed") (fitt + "ed") (fitt + "in")
      } ;

  irregV x y z = let reg = (regV x).s in
    mk5V x (reg ! VPres) y z (reg ! VPresPart) ** {s1 = []} ;

  irreg4V x y z w = let reg = (regV x).s in
    mk5V x (reg ! VPres) y z w ** {s1 = []} ;

  irregDuplV fit y z =
    let
      fitting = (regDuplV fit).s ! VPresPart
    in
    mk5V fit (fit + "s") y z fitting ;

  mkV = overload {
    mkV : (cry : Str) -> V = regV ;
    mkV : (stop, stopped : Str) -> V = reg2V ;
    mkV : (drink, drank, drunk  : Str) -> V = irregV ;
    mkV : (run, ran, run, running  : Str) -> V = irreg4V ;
    mkV : (go, goes, went, gone, going : Str) -> V = mk5V ;
    mkV : Str -> V -> V = prefixV
  };

  mkV2 = overload {
    mkV2  : V -> V2 = dirV2 ;
    mkV2  : Str -> V2 = \s -> dirV2 (regV s) ;
    mkV2  : V -> Prep -> V2 = prepV2 ;
    mkV2  : V -> Str -> V2 = \v,p -> prepV2 v (mkPrep p) ;
    mkV2  : Str -> Prep -> V2 = \v,p -> prepV2 (regV v) p ;
    mkV2  : Str -> Str -> V2 = \v,p -> prepV2 (regV v) (mkPrep p)
  };

  mkV3 = overload {
    mkV3 : V -> Prep -> Prep -> V3 = prepPrepV3 ;
    mkV3 : V -> Prep -> V3 = dirV3 ;
    mkV3 : V -> Str -> V3 = \v,s -> dirV3 v (mkPrep s);
    mkV3 : Str -> Str -> V3 = \v,s -> dirV3 (regV v) (mkPrep s);
    mkV3 : V -> V3 = dirdirV3 ;
    mkV3 : Str -> V3 = \v -> dirdirV3 (regV v) ;
  } ;

  mkV2V = overload {
    mkV2V : Str -> V2V = \s -> lin V2V (dirV2 (regV s) ** {c3 = [] ; typ = VVAux}) ;
    mkV2V : V -> V2V = \v -> lin V2V (dirV2 v ** {c3 = [] ; typ = VVAux}) ;
    mkV2V : V -> Prep -> Prep -> V2V = \v,p,t -> lin V2V (prepV2 v p ** {c3 = t.s ; typ = VVAux}) ;
    } ;
  mkV2Q = overload {
    mkV2Q : V -> Prep -> V2Q = \v,p -> lin V2Q (prepV2 v p) ;
    mkV2Q : V -> V2Q = \v -> lin V2Q (dirV2 v) ;
    mkV2Q : V2 -> V2Q = \v2 -> lin V2Q v2 ;
    mkV2Q : Str -> V2Q = \s -> lin V2Q (dirV2 (regV s)) ;
  } ;

} ;
