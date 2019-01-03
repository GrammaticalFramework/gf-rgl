concrete IdiomPor of Idiom = CatPor **
  open (P = ParamX), MorphoPor, ParadigmsPor, BeschPor, (B = DiffPor), Prelude in {

  flags optimize=all_subs ;

  lin
    ImpersCl vp = mkClause [] True False (agrP3 Masc Sg) vp ;

    GenericCl vp =
      mkClause [] True False (agrP3 Masc Sg) (insertRefl vp) ; ---- just Italian ?

    CleftNP np rs = mkClause [] True False (agrP3 Masc Sg)
      (insertComplement (\\_ => rs.s ! Indic ! np.a)
        (insertComplement (\\_ => (np.s ! rs.c).ton) (predV copula))) ;

    CleftAdv ad s = mkClause [] True False (agrP3 Masc Sg)
      (insertComplement (\\_ => conjThat ++ s.s ! Indic)
        (insertComplement (\\_ => ad.s) (predV copula))) ;


    ExistNP np =
      mkClause [] True False (agrP3 Masc Sg)
      (insertComplement (\\_ => (np.s ! Acc).ton) (predV B.haver_V)) ;

    ExistIP ip = {
      s = \\t,a,p,_ =>
        ip.s ! Nom ++
        (mkClause [] True False (agrP3 Masc Sg) (predV B.haver_V)).s ! DDir ! t ! a ! p ! Indic
      } ;

    ExistNPAdv np adv = mkClause [] True False (agrP3 Masc Sg) (insertComplement (\\_ => (np.s ! Acc).ton ++ adv.s) (predV B.haver_V)) ;

    ExistIPAdv ip adv = {
      s = \\t,a,p,_ =>
        ip.s ! Nom ++
        (mkClause [] True False (agrP3 Masc Sg) (predV B.haver_V)).s ! DDir ! t ! a ! p ! Indic
        ++ adv.s
      } ;

    ProgrVP vp =
      insertComplement
        (\\agr =>
           let
             clpr = <vp.clit1,vp.clit2> ; ----e pronArg agr.n agr.p vp.clAcc vp.clDat ;
             obj  = clpr.p2 ++ vp.comp ! agr ++ vp.ext ! RPos ---- pol
           in
           vp.s.s ! VGer ++ clpr.p1 ++ obj
        )
        (predV B.estar_V) ;

    ImpPl1 vp = {s =
      mkImperative False P1 vp ! RPos ! Masc ! Pl ; --- fem
      } ;

    ImpP3 np vp = {
      s = "deixe" ++ (np.s ! Nom).ton ++ infVP vp np.a ;
        } ;

    SelfAdvVP vp = vp ;

    SelfAdVVP = insertComplement (
      \\agr => case agr of {
        {g = g ; n = n ; p = p} => table {
          P1 => numForms "eu próprio" "nós próprios" ! n ;
          P2 => genNumForms "você mesmo" "você mesma" "vocês mesmos" "vocês mesmas" ! g ! n ;
          P3 => genNumForms "ele próprio" "ela própria" "eles mesmos" "elas mesmas" ! g ! n
          } ! p
        }
      ) ;

    SelfNP np = np ;

} ;
