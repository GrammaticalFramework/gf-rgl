--# -path=.:../../prelude
--# -coding=utf8

--1 A Simple English Resource Morphology
--
-- Aarne Ranta 2002 -- 2005
--
-- This resource morphology contains definitions needed in the resource
-- syntax. To build a lexicon, it is better to use $ParadigmsEng$, which
-- gives a higher-level access to this module.

resource MorphoBul = ResBul ** open
  Predef,
  Prelude,
  CatBul
  in {
  flags coding=utf8 ;


  flags optimize=all ;

oper
--2 Determiners

  mkDeterminerSg = overload {
    mkDeterminerSg : Str -> Str -> Str -> {s : Bool => AGender => Role => Str; nn : NNumber; spec : Species; p : Polarity} = \vseki,vsiaka,vsiako ->
      {s = \\_,g,_  => table AGender [vseki;vseki;vsiaka;vsiako] ! g; nn = NNum Sg; spec = Indef; p = Pos} ;
    mkDeterminerSg : Str -> Str -> Str -> Str -> Str -> Str -> {s : Bool => AGender => Role => Str; nn : NNumber; spec : Species; p : Polarity} = \vseki,vsiaka,vsiako,vseki',vsiaka',vsiako' ->
      {s = \\sp,g,_ => case sp of {
                         True  => table AGender [vseki; vseki; vsiaka; vsiako ] ! g;
                         False => table AGender [vseki';vseki';vsiaka';vsiako'] ! g
                       };
       nn = NNum Sg;
       spec = Indef;
       p = Pos
      } ;
  } ;

  mkDeterminerPl = overload {
    mkDeterminerPl : Str -> {s : Bool => AGender => Role => Str ; nn : NNumber ; spec : Species; p : Polarity} = \vsicki ->
      {s = \\_,_,_ => vsicki; nn = NNum Pl; spec = Indef; p = Pos} ;
    mkDeterminerPl : Str -> Str -> Str -> Str -> {s : Bool => AGender => Role => Str ; nn : NNumber ; spec : Species; p : Polarity} = \i_dvamata,i_dvata,i_dvete,i_dvete_neut ->
      {s = \\_,g,_ => table AGender [i_dvamata;i_dvata;i_dvete;i_dvete_neut] ! g; nn = NNum Pl; spec = Indef; p = Pos} ;
    mkDeterminerPl : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> {s : Bool => AGender => Role => Str ; nn : NNumber ; spec : Species; p : Polarity} = \i_dvamata,i_dvata,i_dvete,i_dvete_neut,i_dvamata',i_dvata',i_dvete',i_dvete_neut' ->
      {s  = \\sp,g,_ => case sp of {
                          True  => table AGender [i_dvamata; i_dvata; i_dvete; i_dvete_neut ] ! g;
                          False => table AGender [i_dvamata';i_dvata';i_dvete';i_dvete_neut'] ! g
                        };
       nn = NNum Pl;
       spec = Indef;
       p = Pos
      } ;
  } ;

  mkQuant = overload {
    mkQuant : Str -> Str -> Str -> Str -> {s : Bool => AForm => Str; nonEmpty : Bool; spec : Species; p : Polarity} = \tozi,tazi,towa,tezi -> 
      { s = \\_ => table {
                     ASg Masc _    => tozi ;
                     ASgMascDefNom => tozi ;
                     ASg Fem  _    => tazi ;
                     ASg Neut _    => towa ;
                     APl      _    => tezi
                   } ;
        nonEmpty = True ;
        spec = Indef ;
        p = Pos
      } ;
      
    mkQuant : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> {s : Bool => AForm => Str; nonEmpty : Bool; spec : Species; p : Polarity} = \tozi,tazi,towa,tezi,tozi',tazi',towa',tezi' -> 
      { s = table {
              True  => table {
                         ASg Masc _    => tozi ;
                         ASgMascDefNom => tozi ;
                         ASg Fem  _    => tazi ;
                         ASg Neut _    => towa ;
                         APl      _    => tezi
                       } ;
              False => table {
                         ASg Masc _    => tozi' ;
                         ASgMascDefNom => tozi' ;
                         ASg Fem  _    => tazi' ;
                         ASg Neut _    => towa' ;
                         APl      _    => tezi'
                       }
            } ;
        nonEmpty = True ;
        spec = Indef ;
        p = Pos
      } ;
  } ;

--2 Verbs

  mkVerb : (_,_,_,_,_,_,_,_,_,_:Str) -> VTable = 
    \cheta,chete,chetoh,chetqh,chel,chetql,cheten,chetqst,cheti,chetene ->
        table {
          VPres      Sg P1 => cheta;
          VPres      Sg P2 => chete + "ш";
          VPres      Sg P3 => chete;
          VPres      Pl P1 => case chete of {
                                _ + ("а"|"я") => chete + "ме";
                                _             => chete + "м"
                              };
          VPres      Pl P2 => chete + "те";
          VPres      Pl P3 => case cheta of {
                                vika + "м" => case chete of {
                                                zn + "ае" => zn + "аят";
                                                dad + "е" => dad + "ат";
                                                vika      => vika + "т"
                                              };
                                _          => cheta + "т"
                              };
          VAorist    Sg P1 => chetoh;
          VAorist    Sg _  => case chetoh of {
                                chet+"ох" => chete;
                                zova+ "х" => zova
                              };
          VAorist    Pl P1 => chetoh + "ме";
          VAorist    Pl P2 => chetoh + "те";
          VAorist    Pl P3 => chetoh + "а";
          VImperfect Sg P1 => chetqh;
          VImperfect Sg _  => case chete of {
                                rabot + "и" => rabot + "eше";
                                _           => chete + "ше"
                              };
          VImperfect Pl P1 => chetqh + "ме";
          VImperfect Pl P2 => chetqh + "те";
          VImperfect Pl P3 => chetqh + "а";
          VPerfect aform   =>let chel1 : Str =
                                   case chel of {
                                     pas+"ъл" => pas+"л";
                                     _        => chel
                                   } ;
                                 chel2 : Str =
                                   case chel of {
                                     w+"лязъл" => w+"лезл";
                                     _         => chel
                                   }
                             in (mkAdjective chel
                                             (chel2+"ия")
                                             (chel2+"ият")
                                             (chel1+"a")
                                             (chel1+"ата")
                                             (chel1+"о")
                                             (chel1+"ото")
                                             (ia2e chel1+"и")
                                             (ia2e chel1+"ите")).s ! aform ;
          VPluPerfect aform => regAdjective chetql  ! aform ;
          VPassive    aform => regAdjective cheten  ! aform ;
          VPresPart   aform => regAdjective chetqst ! aform ;
          VImperative Sg => cheti;
          VImperative Pl => case cheti of {
	                      chet + "и" => chet + "ете";
	                      ela        => ela  + "те"
                            };
          VNoun nform => let v0 = init chetene
                         in (mkNoun (v0+"е")
							        (v0+"ия")
							        (v0+"ия")
							        (v0+"е")
							        ANeut) ! nform;
          VGerund => case chete of {
                       rabot + "и" => rabot + "ейки";
                       _           => chete + "йки"
                     }
        } ;


--2 Nouns

  mkNoun : Str -> Str -> Str -> Str -> AGender -> NForm => Str = \sg,pl,count,voc,g ->
    table {
          NF Sg Indef => sg ;
          NF Sg Def   => case sg of {
                           _+"а"=>sg+"та" ;
                           _+"я"=>sg+"та" ;
                           _+"о"=>sg+"то" ;
                           _+"у"=>sg+"то" ;
                           _+"е"=>sg+"то" ;
                           _+"и"=>sg+"то" ;
                           s+"й"=>s +"я"  ;
                           _+("ър")
                                =>sg +"а" ;
                           _+("тел"|"ар"|"яр"|"ден"
                             |"път"|"огън"|"сън"
                             |"кон"|"крал"|"цар"
                             |"зет"|"лакът"|"нокът")
                                =>sg +"я" ;
                           _    =>case g of {
                                    AFem => sg+"та" ;
                                    _    => sg+"а"
                                  }
                         } ;
          NF Pl Indef => pl ;
          NF Pl Def   => case pl of {
                           _+"а"=>pl+"та" ;
                           _+"е"=>pl+"те" ;
                           _+"и"=>pl+"те" ;
                           _+"я"=>pl+"та" ;
                           _    =>pl+"те"
                         } ;
          NFSgDefNom  => case sg of {
                           _+"а"=>sg+"та" ;
                           _+"я"=>sg+"та" ;
                           _+"о"=>sg+"то" ;
                           _+"у"=>sg+"то" ;
                           _+"е"=>sg+"то" ;
                           _+"и"=>sg+"то" ;
                           s+"й"=>s +"ят" ;
                           _+("ър")
                                =>sg +"ът" ;
                           _+("тел"|"ар"|"яр"|"ден"
                             |"път"|"огън"|"сън"
                             |"кон"|"крал"|"цар"
                             |"зет"|"лакът"|"нокът")
                                =>sg+"ят" ;
                           _    =>case g of {
                                    AFem => sg+"та" ;
                                    _    => sg+"ът"
                                  }
                         } ;
          NFPlCount   => count ;
          NFVocative  => voc
    } ;


--2 Adjectives
    
  mkAdjective : (_,_,_,_,_,_,_,_,_ : Str) -> A = 
          \dobyr,dobria,dobriat,dobra,dobrata,dobro,dobroto,dobri,dobrite -> {
    s = table {
          ASg Masc Indef => dobyr ;
          ASg Masc Def   => dobria ;
          ASgMascDefNom  => dobriat ;
          ASg Fem  Indef => dobra ;
          ASg Fem  Def   => dobrata ;
          ASg Neut Indef => dobro ;
          ASg Neut Def   => dobroto ;
          APl Indef      => dobri ;
          APl Def        => dobrite
        } ;
    adv = dobro ;
    isPre = True ;
    lock_A = <>
    } ;
}
