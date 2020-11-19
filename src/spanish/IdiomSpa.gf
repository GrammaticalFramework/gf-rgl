concrete IdiomSpa of Idiom = CatSpa **
  open (P = ParamX), MorphoSpa, ParadigmsSpa, BeschSpa, (N=NounSpa), Prelude in {

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
        (insertComplement (\\_ => (np.s ! Acc).ton) hay_VP) ;

    ExistNPAdv np adv = ExistNP (N.AdvNP np adv) ;

    ExistIP ip = {
      s = \\t,a,p,_ =>
        ip.s ! Nom ++
        (mkClause [] True False (agrP3 Masc Sg) hay_VP).s ! DDir ! t ! a ! p ! Indic
      } ;

    ExistIPAdv ip adv = {
      s = \\t,a,p,_ =>
        ip.s ! Nom ++
        (mkClause [] True False (agrP3 Masc Sg) hay_VP).s ! DDir ! t ! a ! p ! Indic
        ++ adv.s
      } ;

--    ImpP3 np vp = {} ;

    ProgrVP vp =
      insertComplement
        (\\agr =>
           let
             clpr = <vp.clit1,vp.clit2> ; ----e pronArg agr.n agr.p vp.clAcc vp.clDat ;
             obj  = clpr.p2 ++ vp.comp ! agr ++ vp.ext ! RPos ---- pol
           in
           vp.s.s ! VGer ++ clpr.p1 ++ obj
        )
        (predV (verboV (estar_2 "estar"))) ;

    ImpPl1 vp = {s =
      mkImperative False P1 vp ! RPos ! Masc ! Pl ; --- fem
      } ;

    -- : NP -> NP ;        -- the president himself (is at home)
    -- SelfNP np =

    -- : VP -> VP ;        -- is himself at home
    SelfAdvVP,
    SelfAdVVP = selfVP ;

oper
  hay_VP = predV (verboV (hay_3 "haber")) ;
  selfVP : VP -> VP = insertComplement (
    \\agr => case agr of {
      {g = g ; n = n ; p = p} => table {
        P1 => numForms "yo mismo" "yo misma" ! n ;
        P2 => genNumForms "tu mismo" "tu misma" "vosotros mismos" "vosotras mismas" ! g ! n ;
        P3 => genNumForms "él mismo" "ella misma" "ellos mismos" "ellas mismas" ! g ! n
        } ! p
      }
    ) ;
}
