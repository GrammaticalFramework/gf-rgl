--# -path=.:../abstract:../prelude:../common

-- Adam Slaski, 2011
-- Inari Listenmaa, 2020

   resource ParadigmsPol = open
     CatPol, MorphoPol, ResPol, (NM=NounMorphoPol)
  in
     {
  flags  coding=utf8;

  oper

    mkPN : Str -> (Str -> SubstForm => Str) -> GenNum -> PN ;

    mkA2 : Adj -> Str -> ComplCase -> A2 ;

    mkN : overload {
      mkN : Str -> N ; -- One argument: singular nominative
      mkN : Str -> Gender -> N -- Two arguments: singular nom, gender
      -- TODO: constructors with more than one form
    } ;

--.
-- Definitions hidden from the public API

  mkPN form tab gennum = lin PN
    { nom = (tab form)!SF Sg Nom;
      voc = (tab form)!SF Sg VocP;
      dep = let forms = (tab form) in table {
                  GenPrep|GenNoPrep=>forms!SF Sg Gen;
                  AccPrep|AccNoPrep=>forms!SF Sg Acc;
                  DatPrep|DatNoPrep=>forms!SF Sg Dat;
                  InstrC           =>forms!SF Sg Instr;
                  LocPrep          =>forms!SF Sg Loc};
      gn = gennum ;
      p  = P3
    } ;

    mkA2 adj s c = lin A2 (adj ** { c={s=s; c=c} });

    mkN = overload {
      mkN : Str -> N = mkNGender (Masc Inanimate) ;

      mkN : Str -> Gender -> N = \s,g -> mkNGender g s ;
--      mkN : Str -> Str -> Gender -> N -- Two arguments: singular nom, ???
    } ;

    mkNGender : Gender -> Str -> N = \gen,sgnom ->
     let ntable : SubstForm => Str = guess_paradigm sgnom in
     lin N (NM.mkN ntable gen) ;

    guess_paradigm = overload {
      guess_paradigm : (sgnom : Str) -> SubstForm => Str
      = \sgnom ->  case sgnom of {
          _ + "pospolita" => NM.mkNTable0971 sgnom ;
          _ + "człowiek" => NM.mkNTable0668 sgnom ;
          _ + "ostra" => NM.mkNTable0978 sgnom ;
          _ + "eszcz" => NM.mkNTable0677 sgnom ;
          _ + "dzień" => NM.mkNTable1005 sgnom ;
          _ + "zieł" => NM.mkNTable0805 sgnom ;
          _ + "oźba" => NM.mkNTable0740 sgnom ;
          _ + "ośba" => NM.mkNTable0945 sgnom ;
          _ + "orze" => NM.mkNTable0861 sgnom ;
          _ + "orza" => NM.mkNTable1038 sgnom ;
          _ + "orga" => NM.mkNTable0860 sgnom ;
          _ + "odza" => NM.mkNTable0738 sgnom ;
          _ + "obro" => NM.mkNTable0681 sgnom ;
          _ + "obra" => NM.mkNTable0680 sgnom ;
          _ + "enie" => NM.mkNTable0878 sgnom ;
          _ + "cioł" => NM.mkNTable0796 sgnom ;
          _ + "azdo" => NM.mkNTable0730 sgnom ;
          _ + "azda" => NM.mkNTable0748 sgnom ;
          _ + "atło" => NM.mkNTable0961 sgnom ;
          _ + "asto" => NM.mkNTable0461 sgnom ;
          _ + "asta" => NM.mkNTable0882 sgnom ;
          _ + "anoc" => NM.mkNTable0476 sgnom ;
          _ + "źka" => NM.mkNTable0291 sgnom ;
          _ + "źce" => NM.mkNTable1052 sgnom ;
          _ + "ńko" => NM.mkNTable0876 sgnom ;
          _ + "ńki" => NM.mkNTable0377 sgnom ;
          _ + "ńka" => NM.mkNTable0313 sgnom ;
          _ + "łło" => NM.mkNTable0292 sgnom ;
          _ + "łła" => NM.mkNTable0863 sgnom ;
          _ + "ęto" => NM.mkNTable1051 sgnom ;
          _ + "ęta" => NM.mkNTable0146 sgnom ;
          _ + "ęki" => NM.mkNTable0872 sgnom ;
          _ + "ęka" => NM.mkNTable0974 sgnom ;
          _ + "ęga" => NM.mkNTable0822 sgnom ;
          _ + "ćma" => NM.mkNTable1057 sgnom ;
          _ + "ćki" => NM.mkNTable0164 sgnom ;
          _ + "ćka" => NM.mkNTable0929 sgnom ;
          _ + "ążę" => NM.mkNTable0545 sgnom ;
          _ + "órz" => NM.mkNTable1044 sgnom ;
          _ + "órg" => NM.mkNTable0875 sgnom ;
          _ + "ódź" => NM.mkNTable0745 sgnom ;
          _ + "zie" => NM.mkNTable0841 sgnom ;
          _ + "zia" => NM.mkNTable0072 sgnom ;
          _ + "zen" => NM.mkNTable0620 sgnom ;
          _ + "tno" => NM.mkNTable0958 sgnom ;
          _ + "tha" => NM.mkNTable0275 sgnom ;
          _ + "sto" => NM.mkNTable0089 sgnom ;
          _ + "smo" => NM.mkNTable0660 sgnom ;
          _ + "sia" => NM.mkNTable0036 sgnom ;
          _ + "set" => NM.mkNTable0906 sgnom ;
          _ + "sen" => NM.mkNTable0959 sgnom ;
          _ + "oże" => NM.mkNTable1034 sgnom ;
          _ + "oża" => NM.mkNTable0840 sgnom ;
          _ + "oły" => NM.mkNTable0932 sgnom ;
          _ + "oło" => NM.mkNTable0664 sgnom ;
          _ + "oza" => NM.mkNTable0538 sgnom ;
          _ + "owy" => NM.mkNTable0186 sgnom ;
          _ + "owa" => NM.mkNTable0520 sgnom ;
          _ + "oty" => NM.mkNTable0967 sgnom ;
          _ + "ora" => NM.mkNTable0594 sgnom ;
          _ + "opa" => NM.mkNTable0985 sgnom ;
          _ + "ole" => NM.mkNTable0584 sgnom ;
          _ + "ola" => NM.mkNTable0684 sgnom ;
          _ + "oko" => NM.mkNTable0898 sgnom ;
          _ + "oja" => NM.mkNTable1035 sgnom ;
          _ + "ogi" => NM.mkNTable0690 sgnom ;
          _ + "oga" => NM.mkNTable0689 sgnom ;
          _ + "ody" => NM.mkNTable0195 sgnom ;
          _ + "oda" => NM.mkNTable0293 sgnom ;
          _ + "oba" => NM.mkNTable0636 sgnom ;
          _ + "isa" => NM.mkNTable0409 sgnom ;
          _ + "ieś" => NM.mkNTable0960 sgnom ;
          _ + "iez" => NM.mkNTable0728 sgnom ;
          _ + "iew" => NM.mkNTable0330 sgnom ;
          _ + "eść" => NM.mkNTable0663 sgnom ;
          _ + "esz" => NM.mkNTable1016 sgnom ;
          _ + "est" => NM.mkNTable0638 sgnom ;
          _ + "eni" => NM.mkNTable0968 sgnom ;
          _ + "ele" => NM.mkNTable1003 sgnom ;
          _ + "cko" => NM.mkNTable0703 sgnom ;
          _ + "chy" => NM.mkNTable0482 sgnom ;
          _ + "cco" => NM.mkNTable0980 sgnom ;
          _ + "ało" => NM.mkNTable0536 sgnom ;
          _ + "azd" => NM.mkNTable0682 sgnom ;
          _ + "atr" => NM.mkNTable0895 sgnom ;
          _ + "ato" => NM.mkNTable0833 sgnom ;
          _ + "ara" => NM.mkNTable0851 sgnom ;
          _ + "ły" => NM.mkNTable1019 sgnom ;
          _ + "ąź" => NM.mkNTable0725 sgnom ;
          _ + "ąz" => NM.mkNTable0925 sgnom ;
          _ + "ąg" => NM.mkNTable0819 sgnom ;
          _ + "ąd" => NM.mkNTable0621 sgnom ;
          _ + "óż" => NM.mkNTable0889 sgnom ;
          _ + "zy" => NM.mkNTable0835 sgnom ;
          _ + "zi" => NM.mkNTable0871 sgnom ;
          _ + "ya" => NM.mkNTable0257 sgnom ;
          _ + "wy" => NM.mkNTable0381 sgnom ;
          _ + "tt" => NM.mkNTable0383 sgnom ;
          _ + "ph" => NM.mkNTable0305 sgnom ;
          _ + "on" => NM.mkNTable0716 sgnom ;
          _ + "ny" => NM.mkNTable0445 sgnom ;
          _ + "ma" => NM.mkNTable0783 sgnom ;
          _ + "ki" => NM.mkNTable0261 sgnom ;
          _ + "je" => NM.mkNTable0800 sgnom ;
          _ + "ie" => NM.mkNTable0256 sgnom ;
          _ + "ez" => NM.mkNTable0574 sgnom ;
          _ + "et" => NM.mkNTable0894 sgnom ;
          _ + "ep" => NM.mkNTable0905 sgnom ;
          _ + "em" => NM.mkNTable0877 sgnom ;
          _ + "cy" => NM.mkNTable0387 sgnom ;
          _ + "ch" => NM.mkNTable0201 sgnom ;
          _ + "ba" => NM.mkNTable0999 sgnom ;
          _ + "as" => NM.mkNTable0830 sgnom ;
          _ + "r" => NM.mkNTable0353 sgnom ;
          _ + "k" => NM.mkNTable0025 sgnom ;
          _ + "g" => NM.mkNTable0506 sgnom ;
          _ => NM.mkNTable0000 sgnom -- TODO: other guesses
          }
      -- guess_paradigm : (sgnom, sggen : Str) -> SubstForm => Str
      -- = \sgnom,sggen -> case <sgnom,sggen> of {
      --
      -- }
    };
}
