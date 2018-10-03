concrete StructuralKis of Structural = CatKis ** 
  open MorphoKis, ParadigmsKis,  
  (C = ConstructX), Prelude in {
  flags optimize=all ;
  lin
  above_Prep = mkPrep "juu ya" ;
  after_Prep = mkPrep "baada ya" ;
  all_Predet = {s = \\g => Allpredetprefix g + "ote"} ;
  almost_AdA = mkAdA "karibu " ;
  almost_AdN = mkAdN "nusura " ;
  although_Subj = ss "ingawa" ;
  always_AdV = mkAdV "dawama" ;
  and_Conj = mkConj "na" ;
  because_Subj = ss "maana" ;
  before_Prep = mkPrep "kabla ya"  ;
  behind_Prep = mkPrep "baada" ;
  between_Prep = mkPrep "katikati" ;
  both7and_DConj = mkConj "eli" "na";
  but_PConj = ss "ila" ;
  by8agent_Prep = mkPrep "kwa" ;
  by8means_Prep = mkPrep "kwa" ;
-- can8know_VV, can_VV = {
  during_Prep = mkPrep "katika" ;
  either7or_DConj = mkConj "ama"  singular ;
 everybody_NP = regNP "kila mtu" a_wa singular ;
  every_Det = { s = table {Sub  => "kila"; Obj  g =>[]} ;n = Sg} ; 
  everything_NP = regNP "kila kitu" ki_vi singular ;
  everywhere_Adv = mkAdv "kila sehemu" ;
  few_Det = {s = table {Obj g => Few_prefix g + "chache" ;
             Sub =>   [] };
            n= Pl
             } ;
  for_Prep = mkPrep nonExist ;
  from_Prep = mkPrep "tokea" ;
  he_Pron = mkPron "yeye" "ake" G1 Sg P3 ;
  here_Adv = mkAdv "hapa" ;
  here7to_Adv = mkAdv ["huko"] ;
  here7from_Adv = mkAdv ["hapa"] ;
  how_IAdv = ss "upi" ;
  how8much_IAdv = ss "ngapi" ;
  if_Subj = ss "Kama" ;
  in8front_Prep = mkPrep ["umbele "] ;
 i_Pron   =mkPron "mimi" "angu" G1 Sg P1 ;
  in_Prep = mkPrep "ndani" ;
 it_Pron   ={ s=\\c=>nonExist; poss=\\n,g=> nonExist; a=Ag G4 Sg P3};
 --less_CAdv = C.mkCAdv "kasoro" ;
 much_Det, many_Det = { s = table { 
                      Obj g => Many_prefix g + "ngi" ;
                        Sub =>   []} ;
            n= Pl
             } ;
  more_Adv = mkAdv "zaidi" ; 
  most_Predet = {s = \\g => MorphoKis.Mostpredetprefix g + "ngi"} ;
 --  must_VV = {   
---b  no_Phr = ss "no" ;
  no_Utt = ss "hapana" ;
  on_Prep = mkPrep "juu ya" ;
  one_Quant = {s = \\n,g =>  "moja" } ; 
  only_Predet = {s = \\g =>  "tu" } ;
  or_Conj = mkConj "kana" singular ;
  otherwise_PConj = ss "ila" ;
  please_Voc = ss "tafadhari" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsKis.Number =>   MorphoKis.Gender =>  Str = table {
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
                     in { s= questo};
  quite_Adv = mkAdv "kabisa" ;
  she_Pron  = mkPron "yeye" "ake" G1 Sg P3;
  so_AdA = mkAdA "so" ;
  somebody_NP = regNP "mtu fulani" a_wa singular;
  someSg_Det = { s = table { 
                      Obj g => Detsomesgprefix g + "ngi";
                        Sub =>   []} ;
            n= Sg
             } ;
  somePl_Det = { s = table { 
                      Obj g => Detsomeplprefix g + "ngi" ;
                        Sub =>   []} ;
            n= Pl
             } ;
  something_NP = regNP "kitu fulani" ki_vi singular ;
  somewhere_Adv = mkAdv "seheme fulani" ;
  that_Quant = let
       questo : ParadigmsKis.Number =>  MorphoKis.Gender =>  Str = table {
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
  there_Adv = mkAdv "hapo" ;
  there7to_Adv = mkAdv "hapa kuvika" ;
  there7from_Adv = mkAdv ["hapa kutoka"] ;
  therefore_PConj = ss "kwa ajili" ;
  they_Pron  =mkPron "wao" "ao" G1 Pl P3 ;   
  this_Quant = let
       questo : ParadigmsKis.Number =>  MorphoKis.Gender =>  Str = table {
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
    through_Prep = mkPrep "kuvitila" ;
  too_AdA = mkAdA "vile" ;
  to_Prep = mkPrep "kuvika" ;
  under_Prep = mkPrep "chini ya" ;
  very_AdA = mkAdA "mno" ;
 -- want_VV = mkVV (regV "enda") ;
  we_Pron  =mkPron "sisi" "etu" G1 Pl P1 ;
  --whatPl_IP = mkIP "ata" "ata"  plural ;
 -- whatSg_IP = mkIP "ata" "ata"  singular ;
  when_IAdv = ss "madhali" ;
  when_Subj = ss "madhali" ;
  where_IAdv = ss "wapi" ;
  which_IQuant = {s = \\_ => "wapi"} ;
---b  whichPl_IDet = mkDeterminer plural ["which"] ;
---b  whichSg_IDet = mkDeterminer singular ["which"] ;
 -- whoPl_IP = mkIP "uu" "whom" "whose" plural ;
 -- whoSg_IP = mkIP "who" "whom" "whose" singular ;
  why_IAdv = ss "kwa nini" ;
  without_Prep = mkPrep "bila" ;
  with_Prep = mkPrep "pamoja na" ;
 --yes_Phr = ss "ii" ;
  yes_Utt = ss "ndio" ;
  youSg_Pron  =  mkPron "wewe" "ako" G1 Sg P2 ;
  youPol_Pron,youPl_Pron  = mkPron "nyinyi" "enyu" G1 Pl P3 ;
  not_Predet = {s = \\g =>  "nongi"} ;
  no_Quant =  {s = \\g,n =>  "hakuna"} ;   
  if_then_Conj = mkConj "kama" "basi" singular ;
  nobody_NP = regNP "hakuna mtu" a_wa singular ;
  nothing_NP = regNP "hukuna  kitu" ki_vi singular ;
  at_least_AdN = mkAdN "" ;
  at_most_AdN = mkAdN "kuvika" ;
  except_Prep = mkPrep "kasoro" ;
 -- as_CAdv = C.mkCAdv "kama" ;
 -- have_V2 = dirV2 (mk5V "have" "has" "had" "had" "having") ;
  that_Subj = ss "hio" ;
  lin language_title_Utt = ss "kiswahili" ;
}

