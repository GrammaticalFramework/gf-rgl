concrete ExtraIta of ExtraItaAbs = ExtraRomanceIta ** 
  open CommonRomance, ParadigmsIta, PhonoIta, MorphoIta, ParamX, ResIta, Prelude in {

  lin
    i8fem_Pron = mkPronoun
      "io" "mi" "mi" "me" "me" "mio" "mia" "miei" "mie"
      Fem Sg P1 ;
    these8fem_NP = makeNP ["queste"] Fem Pl ;
    they8fem_Pron = mkPronoun
      "loro" "le" "loro" "glie" "loro" "loro" "loro" "loro" "loro" 
      Fem Pl P3 ;
    this8fem_NP = pn2np (mkPN ["questa"] Fem) ;
    those8fem_NP = makeNP ["quelle"] Fem Pl ;
    we8fem_Pron = 
      mkPronoun "noi" "ci" "ci" "ce" "noi" "nostro" "nostra" "nostri" "nostre"
      Fem Pl P1 ;
    whoPl8fem_IP = {s = \\c => prepCase c ++ "chi" ; a = aagr Fem Pl} ;
    whoSg8fem_IP = {s = \\c => prepCase c ++ "chi" ; a = aagr Fem Sg} ;

    youSg8fem_Pron = mkPronoun 
      "tu" "ti" "ti" "te" "te" "tuo" "tua" "tuoi" "tue"
      Fem Sg P2 ;
    youPl8fem_Pron =
      mkPronoun
        "voi" "vi" "vi" "ve" "voi" "vostro" "vostra" "vostri" "vostre"
        Fem Pl P2 ;
    youPol8fem_Pron =
      mkPronoun
        "Lei" "La" "Le" "Glie" "Lei" "Suo" "Sua" "Suoi" "Sue"
        Fem Sg P3 ;

    youPolPl_Pron = mkPronoun
      "Loro" "Li" "Loro" "Glie" "Loro" "Loro" "Loro" "Loro" "Loro" 
      Masc Pl P3 ;
    youPolPl8fem_Pron = mkPronoun
      "Loro" "Le" "Loro" "Glie" "Loro" "Loro" "Loro" "Loro" "Loro" 
      Fem Pl P3 ;

    PossFamQuant p = {
      s = \\_,n,g,c => case n of {Sg => prepCase c ; _ => possCase g n c} ++ p.poss ! n ! g ;
      sp = \\ n,g,c => case n of {Sg => prepCase c ; _ => possCase g n c} ++ p.poss ! n ! g ;
      s2 = [] ; isNeg = False
      } ;

    AdvDatVP = insertClit3 datClit ;
    AdvGenVP = insertClit3 genClit ;

    ExistsNP np = 
      mkClause [] True False np.a (insertComplement (\\_ => (np.s ! Nom).ton) (predV (regV "esistere"))) ;

    che_cosa_IP = {s = \\c => prepCase c ++ ["che cosa"] ; a = aagr Fem Sg} ;
    cosa_IP = {s = \\c => prepCase c ++ ["cosa"] ; a = aagr Fem Sg} ;

    voiPol_Pron = mkPronoun
       "voi" "vi" "vi" "ve" "voi" "vostro" "vostra" "vostri" "vostre"
       Masc Pl P2
        ** {isPol = True} ;


}
