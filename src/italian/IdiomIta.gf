concrete IdiomIta of Idiom = CatIta ** 
  open (P = ParamX), PhonoIta, MorphoIta, BeschIta, ParadigmsIta, Prelude in {

  flags optimize=all_subs ;

  lin
    ImpersCl vp = mkClause [] True False (agrP3 Masc Sg) vp ;

    GenericCl vp = 
      mkClause [] True False (agrP3 Masc Sg) (insertRefl vp) ;

    CleftNP np rs = mkClause [] True False (agrP3 Masc Sg) 
      (insertComplement (\\_ => rs.s ! Indic ! np.a)
        (insertComplement (\\_ => (np.s ! rs.c).ton) (predV copula))) ;

    CleftAdv ad s = mkClause [] True False (agrP3 Masc Sg) 
      (insertComplement (\\_ => conjThat ++ s.s ! Indic)
        (insertComplement (\\_ => ad.s) (predV copula))) ;

    ExistNP np = 
      let npa = complAgr np.a in
      mkClause [] True False (agrP3 npa.g npa.n) 
        (insertClit3 (elision "ci" "c'" "ci") 
          (insertComplement (\\_ => (np.s ! Nom).ton) 
            (predV copula))) ;

    ExistIP ip = {
      s = \\t,a,p,_ =>
        ip.s ! Nom ++ 
        (mkClause [] True False (agrP3 ip.a.g ip.a.n) 
           (insertClit3 (elision "ci" "c'" "ci") 
              (predV copula))).s ! DDir ! t ! a ! p ! Indic
      } ;

    ProgrVP vp =
      insertComplement 
        (\\agr => 
           let 
             clpr = <[],[],False> ; ----e pronArg agr.n agr.p vp.clAcc vp.clDat ;
             obj  = clpr.p2 ++ vp.comp ! agr ++ vp.ext ! RPos ---- pol
           in
           vp.s.s ! VGer ++ clpr.p1 ++ obj
        )
        (predV (essereV (verboV (stare_16 "stare")))) ;

    ImpPl1 vp = {s =
      mkImperative False P1 vp ! RPos ! Masc ! Pl --- fem
      } ;

}

