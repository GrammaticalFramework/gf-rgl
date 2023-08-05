--# -path=.:../abstract:../prelude:../common

-- Adam Slaski, 2011
-- Inari Listenmaa, 2020

   resource ParadigmsPol = open
     CatPol, MorphoPol, ResPol, (NM=NounMorphoPol), Prelude
  in
     {
  flags  coding=utf8;

  oper

    ComplCase : Type ;
    genPrep : ComplCase ;
    genNoPrep : ComplCase ;
    datPrep : ComplCase ;
    datNoPrep : ComplCase ;

    mkN : overload {
      mkN : Str -> N ; -- One argument: singular nominative
      mkN : Str -> Gender -> N ;-- Two arguments: singular nom, gender
      mkN : Str -> Str -> N  -- Two arguments: sgnom, sggen

    } ;

    mkA2 : A -> Str -> ComplCase -> A2 ;


--.
-- Definitions hidden from the public API

  ComplCase = ResPol.ComplCase ;
  genPrep = GenPrep ;
  genNoPrep = GenNoPrep ;
  datPrep = DatPrep ;
  datNoPrep = DatNoPrep ;

  mkPN : Str -> (Str -> SubstForm => Str) -> GenNum -> PN ;
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
      mkN : Str -> N = mkNGuessGender ;
      mkN : Str -> Gender -> N = mkNGender ;
      mkN : Str -> Str -> N = mkNGuessGender 
      } ;


    -- TODO: Make gender guesser smarter
    mkNGuessGender = overload { 
      -- 1 string
      mkNGuessGender :   (sgnom : Str) -> N = \sgnom ->
        let gender : Gender = case sgnom of {
          _ + "a" => Fem ;
          _ + "ść" => Fem ;
          _ + "noc" => Fem ;
          _ + "wieś" => Fem ;
          _ + "sól" => Fem ;
          _ + "rzecz" => Fem ;
          _ + "o" => Neut ;
          _ + "e" => Neut ;
          _ + "ę" => Neut ;
          _ + "um" => Neut ;
          _ + "nie" => Neut ;
          _       => Masc Inanimate
        } in mkNGender sgnom gender ;
      -- 2 string
      mkNGuessGender :  (sgnom : Str) -> (sggen : Str) -> N = \sgnom,sggen ->
        let gender : Gender = case sgnom of {
          _ + "a" => Fem ;
          _ + "ść" => Fem ;
          _ + "noc" => Fem ;
          _ + "wieś" => Fem ;
          _ + "sól" => Fem ;
          _ + "rzecz" => Fem ;
          _ + "o" => Neut ;
          _ + "e" => Neut ;
          _ + "ę" => Neut ;
          _ + "um" => Neut ;
          _ + "nie" => Neut ;
          _       => Masc Inanimate
        } in mkNGender sgnom sggen gender ;
    };

    mkNGender = overload { 
      -- 1 string
      mkNGender : Str -> Gender -> N = \sgnom,gender ->
        let ntable : SubstForm => Str = guess_paradigm_basic sgnom
        in NM.mkN ntable gender ;
      -- 2 string
      mkNGender : Str -> Str -> Gender -> N = \sgnom,sggen,gender ->
       let ntable : SubstForm => Str = guess_paradigm sgnom
       in NM.mkN ntable gender ;
     };

    guess_paradigm = overload {
      -- 2 string
      guess_paradigm : (sgnom, sggen : Str) -> SubstForm => Str
      = \sgnom,sggen -> case <sgnom,sggen> of {
        <_ + "a",_ + ""> => NM.mkNTable0308 sgnom ;  -- Alternatives: mkNTable0364, mkNTable0644,mkNTable1022,mkNTable0701,mkNTable0189
        <_ + "a",_ + "a"> => NM.mkNTable1045 sgnom ;
        <_ + "a",_ + "i"> => NM.mkNTable0073 sgnom ;  -- Alternatives: mkNTable0287,mkNTable0020,mkNTable0021,mkNTable0055,mkNTable0060,mkNTable0088,mkNTable0254,mkNTable0253,mkNTable0580,mkNTable0921
        <_ + "a",_ + "y"> => NM.mkNTable0021 sgnom ; -- Alternatives: mkNTable0576,mkNTable0530,mkNTable0300,mkNTable0110,mkNTable0411,mkNTable0100,mkNTable0274,mkNTable0302,mkNTable0014,mkNTable0382,mkNTable0099,mkNTable0159,mkNTable0352,mkNTable0161,mkNTable0175,mkNTable0546,mkNTable0565,mkNTable0990,mkNTable0950,mkNTable0760,mkNTable0630,mkNTable0702,mkNTable0721,mkNTable0727
        <_ + "a",_ + "ów"> => NM.mkNTable0501 sgnom ;
        <_ + "a",_ + "ej"> => NM.mkNTable0013 sgnom ;  -- Alternatives: mkNTable0504,mkNTable0755
        <_ + "a",_ + "iej"> => NM.mkNTable0283 sgnom ;
        <_ + "a",_ + "ego"> => NM.mkNTable0614 sgnom ;  
        <_ + "ć",_ + "cia"> => NM.mkNTable0069 sgnom ; -- Alternatives: mkNTable0573,mkNTable0923,mkNTable0922,mkNTable0838,mkNTable0649,mkNTable0734,mkNTable0794,mkNTable0793
        <_ + "ć",_ + "ci"> => NM.mkNTable0475 sgnom ; -- Alternatives: mkNTable0567,mkNTable1014,mkNTable0792,mkNTable0814
        <_ + "ć",_ + ""> => NM.mkNTable0069 sgnom ; -- Alternatives: 
        <_ + "e",_ + ""> => NM.mkNTable0107 sgnom ; 
        <_ + "e",_ + "a"> => NM.mkNTable0413 sgnom ; -- Alternatives: mkNTable0477,mkNTable0836,mkNTable0553
        <_ + "e",_ + "e"> => NM.mkNTable0438 sgnom ; -- Alternatives: mkNTable0527,mkNTable0963,mkNTable0646,mkNTable0714
        <_ + "e",_ + "i"> => NM.mkNTable0081 sgnom ; -- Alternatives: 
        <_ + "e",_ + "u"> => NM.mkNTable0715 sgnom ; -- Alternatives: 
        <_ + "e",_ + "y"> => NM.mkNTable0311 sgnom ; -- Alternatives: 
        <_ + "e",_ + "ów"> => NM.mkNTable0214 sgnom ; -- Alternatives: mkNTable0764
        <_ + "e",_ + "ego"> => NM.mkNTable0472 sgnom ; -- Alternatives: mkNTable0554,mkNTable0694
        <_ + "e",_ + "ych"> => NM.mkNTable0508 sgnom ;
        <_ + "o",_ + "a"> => NM.mkNTable0079 sgnom ; -- Alternatives: mkNTable0158, mkNTable0205,mkNTable0250,mkNTable0265,mkNTable0295,mkNTable0332,mkNTable0388,mkNTable1012,mkNTable1013,mkNTable0618
        <_ + "o",_+"o"> => NM.mkNTable0162 sgnom ; -- Alternatives: mkNTable0182,mkNTable0217,mkNTable0403,mkNTable0918,mkNTable0962
        <_ + "o",_ + "y"> => NM.mkNTable0239 sgnom ;
         -- Alternatives: mkNTable0239
        <_ + "o",_ + "ina"> => NM.mkNTable0083 sgnom ; 
        <_,_ + "a"> => NM.mkNTable0503 sgnom ; -- Alternatives: mkNTable0944,mkNTable0497,mkNTable0244,mkNTable0350,mkNTable0282,mkNTable0197,mkNTable0131
        <_,_ + "i"> => NM.mkNTable0995 sgnom ; -- Alternatives: mkNTable0995,mkNTable0583
        <_,_ + "u"> => NM.mkNTable0171 sgnom ; -- Alternatives: mkNTable0696,mkNTable0111,mkNTable0539,mkNTable0247
        <_,_ + "y"> => NM.mkNTable0550 sgnom ;
        <_,_ + "ia"> => NM.mkNTable0803 sgnom  ;-- Alternative: mkNTable0648
        <_,_ + "iu"> => NM.mkNTable0662 sgnom ;
        <_,_ + "na"> => NM.mkNTable0286 sgnom ;
        <_,_ + "ego"> => NM.mkNTable0589 sgnom ; -- Alternative: mkNTable0966
        <_,_> => guess_paradigm_basic sgnom}; 
     -- 1 string
     guess_paradigm : (sgnom : Str) -> SubstForm => Str = guess_paradigm_basic 
     } ;


    -- Basic 1-str fall-back guesser
    guess_paradigm_basic  : (sgnom : Str) -> SubstForm => Str
    = \sgnom -> case sgnom of {
        -- Non-ambiguous suffixes
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

        -- More alternatives
        _ + "zioł" => NM.mkNTable0328 sgnom ; -- Alternatives: mkNTable0806,mkNTable0807
        _ + "zień" => NM.mkNTable0266 sgnom ; -- Alternatives: mkNTable0707,mkNTable0741,mkNTable0946,mkNTable1023
        _ + "ziec" => NM.mkNTable0534 sgnom ; -- Alternatives: mkNTable0590,mkNTable0776
        _ + "sień" => NM.mkNTable0858 sgnom ; -- Alternatives: mkNTable1025,mkNTable1031
        _ + "siek" => NM.mkNTable0204 sgnom ; -- Alternatives: mkNTable0770,mkNTable0857
        _ + "siec" => NM.mkNTable0607 sgnom ; -- Alternatives: mkNTable0907
        _ + "owie" => NM.mkNTable0208 sgnom ; -- Alternatives: mkNTable0951
        _ + "niek" => NM.mkNTable0140 sgnom ; -- Alternatives: mkNTable0896,mkNTable0899
        _ + "niec" => NM.mkNTable0061 sgnom ; -- Alternatives: mkNTable0210,mkNTable0218,mkNTable0465,mkNTable0470,mkNTable0582
        _ + "cień" => NM.mkNTable0335 sgnom ; -- Alternatives: mkNTable0827
        _ + "ciek" => NM.mkNTable0349 sgnom ; -- Alternatives: mkNTable0588
        _ + "ciec" => NM.mkNTable0666 sgnom ; -- Alternatives: mkNTable0667
        _ + "óźdź" => NM.mkNTable0750 sgnom ; -- Alternatives: mkNTable0751
        _ + "zło" => NM.mkNTable0729 sgnom ; -- Alternatives: mkNTable0753
        _ + "zna" => NM.mkNTable0334 sgnom ; -- Alternatives: mkNTable0873
        _ + "zeł" => NM.mkNTable0398 sgnom ; -- Alternatives: mkNTable0486,mkNTable0742,mkNTable0784,mkNTable0903,mkNTable0904
        _ + "zec" => NM.mkNTable0200 sgnom ; -- Alternatives: mkNTable0356,mkNTable0699,mkNTable0700
        _ + "zda" => NM.mkNTable0602 sgnom ; -- Alternatives: mkNTable0724
        _ + "sło" => NM.mkNTable0299 sgnom ; -- Alternatives: mkNTable0972
        _ + "sta" => NM.mkNTable0098 sgnom ; -- Alternatives: mkNTable0494,mkNTable0532,mkNTable0984
        _ + "seł" => NM.mkNTable0940 sgnom ; -- Alternatives: mkNTable0989
        _ + "śka" => NM.mkNTable0104 sgnom ; -- Alternatives: mkNTable0843,
        _ + "oła" => NM.mkNTable0809 sgnom ; -- Alternatives: mkNTable0910
        _ + "ota" => NM.mkNTable0652 sgnom ; -- Alternatives: mkNTable1024
        _ + "óbr" => NM.mkNTable0178 sgnom ; -- Alternatives: mkNTable0625
        _ + "nie" => NM.mkNTable0402 sgnom ; -- Alternatives: mkNTable0507
        _ + "nia" => NM.mkNTable0035 sgnom ; -- Alternatives: mkNTable0317,mkNTable0339,mkNTable0688,mkNTable0986
        _ + "ień" => NM.mkNTable0563 sgnom ; -- Alternatives: mkNTable0947,mkNTable0948,mkNTable0976,mkNTable0998
        _ + "ieł" => NM.mkNTable0456 sgnom ; -- Alternatives: mkNTable0789,mkNTable0790
        _ + "ieć" => NM.mkNTable0704 sgnom ; -- Alternatives: mkNTable0795,mkNTable0926,mkNTable0927
        _ + "ies" => NM.mkNTable0913 sgnom ; -- Alternatives: mkNTable0934
        _ + "ier" => NM.mkNTable0485 sgnom ; -- Alternatives: mkNTable0493,mkNTable0525,mkNTable0718,mkNTable0993
        _ + "iel" => NM.mkNTable0544 sgnom ; -- Alternatives: mkNTable0557,mkNTable0566,mkNTable0593,mkNTable0629,mkNTable0844,mkNTable0977
        _ + "iec" => NM.mkNTable0016 sgnom ; -- Alternatives: mkNTable0157,mkNTable0390,mkNTable0524,mkNTable0577,mkNTable0640,mkNTable0832,mkNTable0897
        _ + "ech" => NM.mkNTable0374 sgnom ; -- Alternatives: mkNTable0847
        _ + "cia" => NM.mkNTable0232 sgnom ; -- Alternatives: mkNTable0891
        _ + "cho" => NM.mkNTable1007 sgnom ; -- Alternatives: mkNTable1007a,ci,mkNTable0087,mkNTable0812
        _ + "cha" => NM.mkNTable0198 sgnom ; -- Alternatives: mkNTable0378
        _ + "ądź" => NM.mkNTable1053 sgnom ; -- Alternatives: mkNTable1054,mkNTable1055
        _ + "ądz" => NM.mkNTable0821 sgnom ; -- Alternatives: mkNTable0931
        _ + "zd" => NM.mkNTable0222 sgnom ; -- Alternatives: mkNTable0692,mkNTable0749
        _ + "za" => NM.mkNTable0752 sgnom ; -- Alternatives: mkNTable1048
        _ + "wa" => NM.mkNTable0585 sgnom ; -- Alternatives: mkNTable0693,mkNTable0936
        _ + "ół" => NM.mkNTable0421 sgnom ; -- Alternatives: mkNTable0568,mkNTable0653,mkNTable0712,mkNTable0813,mkNTable0902,mkNTable0939
        _ + "ów" => NM.mkNTable0011 sgnom ; -- Alternatives: mkNTable0049,mkNTable0642,mkNTable0754,mkNTable0797,mkNTable0888,mkNTable1040
        _ + "ót" => NM.mkNTable0386 sgnom ; -- Alternatives: mkNTable0811
        _ + "ór" => NM.mkNTable0179 sgnom ; -- Alternatives: mkNTable0466,mkNTable0579,mkNTable0683,mkNTable0915
        _ + "ól" => NM.mkNTable0441 sgnom ; -- Alternatives: mkNTable0763,mkNTable0874
        _ + "ój" => NM.mkNTable0458 sgnom ; -- Alternatives: mkNTable0526,mkNTable0731,mkNTable0808,mkNTable0938,mkNTable0997
        _ + "óg" => NM.mkNTable0193 sgnom ; -- Alternatives: mkNTable0399,mkNTable0400,mkNTable0606,mkNTable0626
        _ + "ód" => NM.mkNTable0610 sgnom ; -- Alternatives: mkNTable0743,mkNTable0942
        _ + "ób" => NM.mkNTable0609 sgnom ; -- Alternatives: mkNTable0624
        _ + "um" => NM.mkNTable0095 sgnom ; -- Alternatives: mkNTable0839
        _ + "to" => NM.mkNTable0042 sgnom ; -- Alternatives: mkNTable0337,mkNTable0511,mkNTable0955
        _ + "th" => NM.mkNTable0237 sgnom ; -- Alternatives: mkNTable0263
        _ + "ta" => NM.mkNTable0026 sgnom ; -- Alternatives: mkNTable0103,mkNTable0153,mkNTable0262,mkNTable0496
        _ + "sł" => NM.mkNTable0155 sgnom ; -- Alternatives: mkNTable0685
        _ + "st" => NM.mkNTable0097 sgnom ; -- Alternatives: mkNTable0510,mkNTable0549,mkNTable0824,mkNTable0855
        _ + "ro" => NM.mkNTable0410 sgnom ; -- Alternatives: mkNTable0537
        _ + "ra" => NM.mkNTable0048 sgnom ; -- Alternatives: mkNTable0736
        _ + "oł" => NM.mkNTable0077 sgnom ; -- Alternatives: mkNTable0531
        _ + "no" => NM.mkNTable0555 sgnom ; -- Alternatives: mkNTable0575
        _ + "ni" => NM.mkNTable0498 sgnom ; -- Alternatives: mkNTable0547
        _ + "na" => NM.mkNTable0144 sgnom ; -- Alternatives: mkNTable0615
        _ + "ło" => NM.mkNTable0492 sgnom ; -- Alternatives: mkNTable0543,mkNTable0651,mkNTable1039
        _ + "ła" => NM.mkNTable0152 sgnom ; -- Alternatives: mkNTable0169,mkNTable0632,mkNTable0747,mkNTable0778,mkNTable0779,mkNTable0992
        _ + "ko" => NM.mkNTable0133 sgnom ; -- Alternatives: mkNTable0199
        _ + "ka" => NM.mkNTable0006 sgnom ; -- Alternatives: mkNTable0027,mkNTable0126,mkNTable0439,mkNTable0581,mkNTable0669,mkNTable0849
        _ + "ja" => NM.mkNTable0059 sgnom ; -- Alternatives: mkNTable0094,mkNTable0121,mkNTable0241,mkNTable0720
        _ + "in" => NM.mkNTable0029 sgnom ; -- Alternatives: mkNTable0102
        _ + "ia" => NM.mkNTable0022 sgnom ; -- Alternatives: mkNTable0490
        _ + "t" => NM.mkNTable0033 sgnom ; -- Alternatives: mkNTable0190,mkNTable0231,mkNTable0233,mkNTable0502,mkNTable0599,mkNTable0658,mkNTable0659,mkNTable0723,mkNTable0804
        _ + "ha" => NM.mkNTable0429 sgnom ; -- Alternatives: mkNTable0597,mkNTable0695
        _ + "ga" => NM.mkNTable0030 sgnom ; -- Alternatives: mkNTable0219,mkNTable0798
        _ + "eń" => NM.mkNTable0697 sgnom ; -- Alternatives: mkNTable0698,mkNTable0911,mkNTable0988,mkNTable1001,mkNTable1008
        _ + "eł" => NM.mkNTable0246 sgnom ; -- Alternatives: mkNTable0678,mkNTable0679,mkNTable0823,mkNTable1028
        _ + "eć" => NM.mkNTable0780 sgnom ; -- Alternatives: mkNTable0781,mkNTable0802,mkNTable0818,mkNTable0956,mkNTable0957,mkNTable1011
        _ + "ew" => NM.mkNTable0342 sgnom ; -- Alternatives: mkNTable0343,mkNTable0600,mkNTable0801,mkNTable0943
        _ + "er" => NM.mkNTable0047 sgnom ; -- Alternatives: mkNTable0278,mkNTable0469,mkNTable0517,mkNTable0548,mkNTable1009
        _ + "en" => NM.mkNTable0587 sgnom ; -- Alternatives: mkNTable0622,mkNTable0713
        _ + "el" => NM.mkNTable0009 sgnom ; -- Alternatives: mkNTable0167,mkNTable0513,mkNTable0558,mkNTable0571,mkNTable0619,mkNTable0657,mkNTable0675,mkNTable0762,mkNTable0785,mkNTable0881
        _ + "ek" => NM.mkNTable0046 sgnom ; -- Alternatives: mkNTable0435,mkNTable0463,mkNTable0512
        _ + "ec" => NM.mkNTable0154 sgnom ; -- Alternatives: mkNTable0301,mkNTable0360,mkNTable0366,mkNTable0448,mkNTable0591,mkNTable0817
        _ + "eb" => NM.mkNTable1046 sgnom ; -- Alternatives: mkNTable1047
        _ + "co" => NM.mkNTable0238 sgnom ; -- Alternatives: mkNTable0768,mkNTable0831
        _ + "ce" => NM.mkNTable0345 sgnom ; -- Alternatives: mkNTable0433
        _ + "ca" => NM.mkNTable0432 sgnom ; -- Alternatives: mkNTable0631,mkNTable0767,mkNTable0879,mkNTable0912
        _ + "at" => NM.mkNTable0559 sgnom ; -- Alternatives: mkNTable0854,mkNTable1027
        _ + "ad" => NM.mkNTable0890 sgnom ; -- Alternatives: mkNTable0996
        _ + "ąż" => NM.mkNTable0870 sgnom ; -- Alternatives: mkNTable1010,mkNTable1029,mkNTable1030
        _ + "ąc" => NM.mkNTable0853 sgnom ; -- Alternatives: mkNTable1006
        _ + "ąb" => NM.mkNTable0259 sgnom ; -- Alternatives: mkNTable0260,mkNTable0706,mkNTable0710,mkNTable0711,mkNTable0733,mkNTable0756,mkNTable0769
        _ + "o" => NM.mkNTable0051 sgnom ;
        -- NB: Covered in 2-string, including alternatives: mkNTable0079,mkNTable0083,mkNTable0158,mkNTable0162,mkNTable0182,mkNTable0205,mkNTable0217,mkNTable0239,mkNTable0250,mkNTable0265,mkNTable0295,mkNTable0332,mkNTable0388,mkNTable0403,mkNTable0418,mkNTable0420,mkNTable0459,mkNTable0460,mkNTable0488,mkNTable0499,mkNTable0509,mkNTable0521,mkNTable0541,mkNTable0561,mkNTable0562,mkNTable0605,mkNTable0608,mkNTable0611,mkNTable0618,mkNTable0645,mkNTable0654,mkNTable0655,mkNTable0719,mkNTable0829,mkNTable0842,mkNTable0900,mkNTable0918,mkNTable0962,mkNTable1012,mkNTable1013
        _ + "j" => NM.mkNTable0799 sgnom ; -- Alternatives: mkNTable0834
        _ + "i" => NM.mkNTable0054 sgnom ; -- Alternatives: mkNTable0068,mkNTable0134,mkNTable0147,mkNTable0825
        _ + "e" => NM.mkNTable0081 sgnom ;
        -- NB: Covered in 2-string, including  alternatives: mkNTable0107,mkNTable0214,mkNTable0311,mkNTable0413,mkNTable0438,mkNTable0472,mkNTable0477,mkNTable0508,mkNTable0527,mkNTable0553,mkNTable0554,mkNTable0646,mkNTable0694,mkNTable0714,mkNTable0715,mkNTable0764,mkNTable0836,mkNTable0963
        _ + "ś" => NM.mkNTable0019 sgnom ; -- Alternatives: mkNTable0080,mkNTable0258,mkNTable0603,mkNTable0634,mkNTable0671,mkNTable0757,mkNTable0782,mkNTable0887,mkNTable0916,mkNTable0953
        _ + "ń" => NM.mkNTable0142 sgnom ; -- Alternatives: mkNTable0268,mkNTable0290,mkNTable0297,mkNTable0468,mkNTable0592,mkNTable0612,mkNTable0674,mkNTable0676,mkNTable0775,mkNTable0815,mkNTable0935,mkNTable1004
        _ + "ł" => NM.mkNTable0151 sgnom ; -- Alternatives: mkNTable0192,mkNTable0280,mkNTable0533,mkNTable0601
        _ + "ę" => NM.mkNTable0379 sgnom ; -- Alternatives: mkNTable0604
        _ + "ć" => NM.mkNTable0069 sgnom ; -- Alternatives: mkNTable0475,mkNTable0567,mkNTable0573,mkNTable0649,mkNTable0734,mkNTable0792,mkNTable0793,mkNTable0794,mkNTable0814,mkNTable0838,mkNTable0922,mkNTable0923,mkNTable1014
        _ + "ź" => NM.mkNTable0316 sgnom ; -- Alternatives: mkNTable0633,mkNTable0661,mkNTable0722,mkNTable0732,mkNTable0771
        _ + "y" => NM.mkNTable0012 sgnom ; -- Alternatives: mkNTable0050,mkNTable0058,mkNTable0123,mkNTable0203,mkNTable0635,mkNTable0665,mkNTable0777,mkNTable0886,mkNTable1020
        _ + "x" => NM.mkNTable0038 sgnom ; -- Alternatives: mkNTable0230,mkNTable0848
        _ + "c" => NM.mkNTable0114 sgnom ; -- NB: Covered in 2-string, including all alternatives: mkNTable0505,mkNTable0628,mkNTable0919
        _ + "a" => NM.mkNTable0021 sgnom ;  
        -- NB: Covered in 2-string, including all alternatives: mkNTable0308,mkNTable0364,mkNTable0644,mkNTable0701,mkNTable1022,mkNTable0013,mkNTable0014,mkNTable0020,mkNTable0021,mkNTable0055,mkNTable0060,mkNTable0073,mkNTable0088,mkNTable0099,mkNTable0100,mkNTable0110,mkNTable0159,mkNTable0161,mkNTable0175,mkNTable0189,mkNTable0253,mkNTable0254,mkNTable0274,mkNTable0283,mkNTable0287,mkNTable0300,mkNTable0302,mkNTable0352,mkNTable0382,mkNTable0411,mkNTable0501,mkNTable0504,mkNTable0530,mkNTable0546,mkNTable0565,mkNTable0576,mkNTable0580,mkNTable0614,mkNTable0630,mkNTable0702,mkNTable0721,mkNTable0727,mkNTable0755,mkNTable0760,mkNTable0921,mkNTable0950,mkNTable0990,mkNTable1045
        _ => NM.mkNTable0171 sgnom  -- Alternatives: mkNTable0000,mkNTable0001,mkNTable0002,mkNTable0003,mkNTable0010,mkNTable0015,mkNTable0028,mkNTable0037,mkNTable0043,mkNTable0044,mkNTable0053,mkNTable0064,mkNTable0067,mkNTable0075,mkNTable0091,mkNTable0096,mkNTable0111,mkNTable0117,mkNTable0118,mkNTable0129,mkNTable0131,mkNTable0168,mkNTable0171,mkNTable0173,mkNTable0176,mkNTable0181,mkNTable0191,mkNTable0197,mkNTable0213,mkNTable0243,mkNTable0244,mkNTable0247,mkNTable0248,mkNTable0271,mkNTable0281,mkNTable0282,mkNTable0286,mkNTable0304,mkNTable0309,mkNTable0312,mkNTable0315,mkNTable0324,mkNTable0333,mkNTable0338,mkNTable0348,mkNTable0350,mkNTable0365,mkNTable0373,mkNTable0375,mkNTable0428,mkNTable0444,mkNTable0467,mkNTable0495,mkNTable0497,mkNTable0500,mkNTable0503,mkNTable0514,mkNTable0516,mkNTable0518,mkNTable0519,mkNTable0523,mkNTable0539,mkNTable0542,mkNTable0550,mkNTable0552,mkNTable0570,mkNTable0578,mkNTable0583,mkNTable0589,mkNTable0648,mkNTable0662,mkNTable0691,mkNTable0696,mkNTable0717,mkNTable0773,mkNTable0803,mkNTable0826,mkNTable0828,mkNTable0859,mkNTable0868,mkNTable0869,mkNTable0944,mkNTable0964,mkNTable0965,mkNTable0966,mkNTable0970,mkNTable0981,mkNTable0991,mkNTable0995
     } ;

  mkMU : Str -> MU = \s -> lin MU {s=s; isPre=False} ;

}
