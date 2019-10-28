concrete StructuralKis of Structural = CatKis ** 
  open MorphoKis, ParadigmsKis, DiffKis, 
  (C = ConstructX), Prelude in {
  flags optimize=all ;
  lin
  above_Prep = mkPrep "juu ya" False;
  after_Prep = mkPrep "baada ya" False;
  all_Predet = {s = \\g => Allpredetprefix g + "ote"} ;
  almost_AdA = mkAdA "karibu " ;
  almost_AdN = mkAdN "nusura " ;
  although_Subj = ss "ingawa" ;
  always_AdV = mkAdV "daima" ;
  and_Conj = mkConj "na" ;
  because_Subj = ss "maana" | ss "sababu"| ss "kwa sababu";
  before_Prep = mkPrep "kabla ya" False ;
  behind_Prep = mkPrep "baada" False;
  between_Prep = mkPrep "katikati"False ;
  both7and_DConj = mkConj "eli" "na";
  but_PConj = ss "ila" ;
  by8agent_Prep = mkPrep "na"False ;
  by8means_Prep = mkPrep "kwa"False ;
 can8know_VV, can_VV = MorphoKis.regV "weza";
  during_Prep = mkPrep "wakati wa" False;
  either7or_DConj = mkConj "ima" "au" singular ;
 everybody_NP = regNP "kila mtu" a_wa singular  False;
  every_Det = { s =\\g => "kila"; n = Sg; isPre =False} ; 
  everything_NP = regNP "kila kitu" ki_vi singular False ;
  everywhere_Adv = mkAdv "kila mahali" ;
 
  few_Det = {s =\\g => Few_prefix g + "chache" ; n= Pl; isPre=True};
             
  for_Prep = mkPrep nonExist False;
  from_Prep = mkPrep "ya"False | mkPrep "kutoka" False;
  he_Pron = mkPron "yeye" "ake" G1 Sg P3 ;
  here_Adv = mkAdv "hapa" ;
  here7to_Adv = mkAdv ["huko"] ;
  here7from_Adv = mkAdv ["hapa"] ;
  how_IAdv = ss "je" ;
  how8much_IAdv = ss "ngapi" ; --check with how many
  if_Subj = ss "Kama"  ;
  in8front_Prep = mkPrep ["umbele "] False;
 i_Pron   =mkPron "mimi" "angu" G1 Sg P1 ;
  in_Prep = mkPrep "kwenye" True;
 it_Pron   ={ s=\\c=>nonExist; poss=\\n,g=> nonExist; a=Ag G4 Sg P3};
 more_CAdv =C.mkCAdv "zaidi" "";
 --less_CAdv = C.mkCAdv "kasoro" ;
 much_Det, many_Det = { s =\\g =>  Many_prefix g + "ngi" ; n= Pl; isPre =True };

  more_Adv = mkAdv "zaidi" ; 
  most_Predet = {s = \\g => MorphoKis.Mostpredetprefix g + "ngi"} ;
 --  must_VV = {   
  no_Phr = ss "hapana" ;
  no_Utt = ss "hapana" ;
  on_Prep = mkPrep "kwenye" True | mkPrep "kuhusu"  True;
  one_Quant = {s = \\n,g =>  "moja" } ; 
  only_Predet = {s = \\g =>  "tu" } ;
  or_Conj = mkConj "kana" singular ;
  otherwise_PConj = ss "ila" ;
  please_Voc = ss "tafadhari" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsKis.Number =>   MorphoKis.Cgender =>  Str = table {
    Sg => \\g=> case <g> of {    <G3> => "la" ; 
                                 <G4> => "cha" ; 
                                 <G5> => "ya" ; 
                                 <G11> => "pa";
                                 <G12> => "kwa";
                                 <G13> => "mwa";
                                 <G1> |<G2>|<G6>|<G7> |<G8> => "wa" ; 
                                  _ => ""}; 
                       
      Pl => \\g=> case <g> of{<G1> => "wa" ; 
                              <G4> => "vya" ; 
                              <G5>|<G6> => "za" ; 
                              <G2>|<G3> |<G8> |<G9> |<G10> => "ya"; 
                                 _ => ""}}
                     in { s= questo; isFused= False};
  quite_Adv = mkAdA "kabisa" ;
  she_Pron  = mkPron "yeye" "ake" G1 Sg P3;
  so_AdA = mkAdA "so" ;
  somebody_NP = regNP "mtu mwingine" a_wa singular False;
  someSg_Det = { s =\\g =>  Detsomesgprefix g + "ngine"; n= Sg; isPre=True  } ;
  somePl_Det = { s =\\g => Detsomeplprefix g + "ngine" ; n= Pl; isPre=True } ;
  something_NP = regNP "kitu fulani" ki_vi singular False;
  somewhere_Adv = mkAdv "sehemu fulani" ;
  that_Quant = let
       questo : ParadigmsKis.Number =>  MorphoKis.Cgender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G1> => "huyo" ;
                    <G2 > => "huo";
                    <G3 > => "hilo";
                    <G4 > => "hicho";
                    <G5 > => "hiyo";
                    <G6 > => "huo";
                    <G7> => "huo";
                    <G8> => "huo" ;
                    <G11 > => "hapo";
                    <G12 > => "huko";
                    <G13 > => "humo";
                    <G9 > | <G10 >=> ""          
                   }; 
                       
      Pl => \\g=> case <g> of{
                    <G1> => "hao" ;
                    <G2 > => "hiyo";
                    <G3 > => "hayo";
                    <G4 > => "hivyo";
                    <G5 > => "hizo";
                    <G6 > => "hizo";
                    <G8> => "hayo" ;
                    <G10 > => "hiyo";
                    <G12 > => "huko";
                    <G13 > => "humo";
                   _ => ""
                   } 
               
           };
    in {
         s = questo ;
        } ;
  there_Adv = mkAdv "pale" ;
  there7to_Adv = mkAdv "huko " ;
  there7from_Adv = mkAdv [" kutoka pale"] ;
  therefore_PConj = ss "kwa hivyo" ;
  they_Pron  =mkPron "wao" "ao" G1 Pl P3 ;   
  this_Quant = let
       questo : ParadigmsKis.Number =>  MorphoKis.Cgender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G1> => "huyu" ;
                    <G11> => "hapa" ;
                    <G12> => "huku" ;
                    <G13> => "humu" ;
                    <G4 > => "hiki";
                    <G3 > => "hili";
                    <G5 > => "hii";
                    <G9 > |<G10 >=> "";
                    <G2 > |<G8>|<G7> |<G6> => "huu"        
                   }; 
                       
      Pl => \\g=> case <g> of{                  
                    <G1> => "hawa" ;
                    <G4 > => "hivi";
                    <G5 > |<G6 > => "hizi";
                    <G2 > |<G10> => "hii";
                     <G3 > |<G8 >|<G9 > => "haya";
                      _=> ""
                   } 
               
           }
    in {
         s = questo ;
        } ;
    through_Prep = mkPrep "kuvitia" False;
  too_AdA = mkAdA "vile" ;
  to_Prep = mkPrep "hadi" False ;
  under_Prep = mkPrep "chini ya"False ;
  very_AdA = mkAdA "sana" ;
  want_VV = MorphoKis.regV  "taka" ;
  we_Pron  =mkPron "sisi" "etu" G1 Pl P1 ;
  whatPl_IP = mkIP "nini"  plural ;
  whatSg_IP = mkIP "nini"  singular ;
  when_IAdv = ss "lini" ;
  when_Subj = ss "wakati" ;
  where_IAdv = ss "wapi" ;
  which_IQuant = let
       questo : ParadigmsKis.Number =>  DiffKis.Cgender =>  Str =
        table {
    Sg => \\g=> IDetprefixsg  g + "pi"   ;
    Pl => \\g=> IDetprefixpl g + "pi" };
        in { s = questo ; } ;
  how8many_IDet= {s=\\g=> PrefixPlNom g + "ngapi" ; n=Pl} ;
  whichPl_IDet = {s=\\g=> "amba" + IQuantprefixpl g  ; n=Pl} ;
  whichSg_IDet = {s=\\g=> "amba" + IQuantprefixsg g ; n=Sg} ;
 whoPl_IP = mkIP "nani" plural ;
  whoSg_IP = mkIP "nani" singular ;
  why_IAdv = ss "kwa nini" ;
  without_Prep = mkPrep "bila" False ;
  with_Prep = mkPrep "wenye" False ;
 yes_Phr = ss "ndio" ;
  yes_Utt = ss "ndio" ;
  youSg_Pron  =  mkPron "wewe" "ako" G1 Sg P2 ;
  youPol_Pron,youPl_Pron  = mkPron "nyinyi" "enyu" G1 Pl P2 ;
  not_Predet = {s = \\g =>  "nongi"} ;
  no_Quant =  {s = \\g,n =>  "hakuna"} ;   
  if_then_Conj = mkConj "kama" "basi" singular ;
  nobody_NP = regNP "hakuna mtu" a_wa singular False;
  nothing_NP = regNP "hukuna  kitu" ki_vi singular False ;
  at_least_AdN = mkAdN "" ;
  at_most_AdN = mkAdN "kuvika" ;
  except_Prep = mkPrep "kasoro" False ;
  as_CAdv = C.mkCAdv "kama" [] ;
  have_V2 = dirV2 (mkV "kuwa na") ;
  that_Subj = ss "hio" ;
  lin language_title_Utt = ss "kiswahili" ;
}

