concrete StructuralGus of Structural = CatGus ** 
  open MorphoGus, ParadigmsGus, 
   (C = ConstructX), Prelude in {

  flags optimize=all ;

  lin
  above_Prep = mkPrep "igoro" ;
  after_Prep = mkPrep "itina" ;
  all_Predet = {s = \\g => MorphoGus.Allpredetprefix g + "onsi"} ;
  almost_AdA = mkAdA "ang'e " ;
  almost_AdN = mkAdN "vakuvi " ;
  although_Subj = ss "ona kau" ;
  always_AdV = mkAdV "botambe " ;
  and_Conj = mkConj "na" ;
  because_Subj = ss "nundu" | ss "ni kwithiwa" ;
  before_Prep = mkPrep "mbee" |mkPrep "vau mbeange" ;
  behind_Prep = mkPrep "itina" ;
  between_Prep = mkPrep "kati" ;
  both7and_DConj = mkConj "eli" "na";
  but_PConj = ss "korende" ;
  by8agent_Prep = mkPrep "kwa" ;
  by8means_Prep = mkPrep "kwa" ;

 during_Prep = mkPrep "during" ;
  either7or_DConj = mkConj "kana"  singular ;
 everybody_NP = regNP "kila umwe" G1 singular ;
  every_Det =  {s = table {Obj g => "" ;
             Sub =>   "kila" };
            n= Sg
             } ;
 
  -- mkDeterminerSpec singular "every" "everyone" False ;
  everything_NP = regNP "ase oboamo" G1 singular ; --gender confirm
  everywhere_Adv = mkAdv "ase oboamo" ;
  few_Det =  {s = table {Obj g => Few_prefix g + "nini" ;
             Sub =>   [] };
            n= Pl
             } ;
  -- mkDeterminer plural "few" ;
---  first_Ord = ss "first" ; DEPRECATED
  for_Prep = mkPrep "aera" ;
  from_Prep = mkPrep "kuma" ;
  he_Pron = mkPron "ere" "je" G1 Sg P3 ;
  here_Adv = mkAdv "vaa" ;
  here7to_Adv = mkAdv ["kuvika vaa"] ;
  here7from_Adv = mkAdv ["kuma vaa"] ;
  how_IAdv = ss "ata" | ss "nzia myau" ;
  how8much_IAdv = ss "mala meana" ;
 --how8many_IDet = mkDeterminer plural ["mala meana"] ;
  if_Subj = ss "enthwa" ;
  in8front_Prep = mkPrep ["mbee wa"] ;
 i_Pron   =mkPron "inche" "ne" G1 Sg P1 ;
  in_Prep = mkPrep "in" ;
 it_Pron   ={ s=\\c=>"yo"; poss=\\n,g=> ""; a=Ag G4 Sg P3};-- mkPron "yo" ""G4 Sg P3  ;
       
  less_CAdv = C.mkCAdv "ninangi" "kwi" ;
 much_Det, many_Det = { s = table { 
                      Obj g => Many_prefix g + "nge" ;
                        Sub =>   []} ;
            n= Pl
             } ;
  more_CAdv = C.mkCAdv "mbeange" "kwi" ; -- more should be a function beccause og Gender
 -- most_Predet = {s = \\g => MorphoGus.Mostpredetprefix g + "ingi"} ;
 
{-}  must_VV = {
    s = table {
      VVF VInf => ["have to"] ;
      VVF VPres => "must" ;
      VVF VPPart => ["had to"] ;
      VVF VPres2Part => ["having to"] ;
      VVF VPast => ["had to"] ;      --# notpresent
      VVPastNeg => ["hadn't to"] ;      --# notpresent
      VVPresNeg => "mustn't"
      } ;
    p = [] ;
    typ = VVAux
    
    } ; -}
---b  no_Phr = ss "no" ;
  no_Utt = ss "yaya" ;
  on_Prep = mkPrep "iulu wa" ;
----  one_Quant = mkDeterminer singular "one" ; -- DEPRECATED
  only_Predet = {s = \\g =>  "tu" } ;
  or_Conj = mkConj "kana" singular ;
  otherwise_PConj = ss "otherwise" ;
--part_Prep = mkPrep "" ;
  please_Voc = ss "ame" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsGus.Number =>  MorphoGus.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G1>| <G2 > => "bwo" ;
                    <G3 > => "ya";
                    <G4 > => "ria";
                    <G5 > => "kia"; --
                    <G6 > => "rwa";
                    <G7> => "ka";
                    <G8> => "bwa";
                    <G9> => "kwa";
                    <G10> => "a"
                   }; 
                       
      Pl => \\g=> case <g> of{
                   <G1> => "ba" ;
                    <G2 > => "ya" ;
                    <G3 >|<G6 > => "cia";
                    <G4 > |<G8>|<G9>|<G10> => "a";
                    <G5 > => "bi"; --
                    <G7> => "bia"
                       }                
           }
    in { s= questo} ;
  quite_Adv = mkAdv "o muno" ;
  she_Pron  = mkPron "ere" "je" G1 Sg P3 ;
           
  so_AdA = mkAdA "so" ;
  somebody_NP = regNP " o mundu " G1 singular ;
  someSg_Det = { s = table  {Obj g  => Some_prefix g  ;
             Sub =>   [] } ;
            n= Sg
             } ;
  somePl_Det = {s = table  {Obj g => Some_prefix g ;
               Sub =>   [] };
            n= Pl
             } ;
  something_NP = regNP "o kindu" G1 singular ; --confirm gender
  somewhere_Adv = mkAdv "o vandu" ;
  that_Quant = let
       questo : ParadigmsGus.Number => MorphoGus.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                  <G1> => "aria";
                  <G2> => "oria";
                  <G4> => "riira";
                  <G5> => "keria";
                  <G3> => "eria";
                  <G6> => "roria";
                  <G7> => "karia";
                  <G8> => "boria";
                  <G9> => "koria";
                  <G10> => "aria"
                   }; 
                       
      Pl => \\g=> case <g> of{
                  <G1> => "baria" |"abuo";
                  <G2> => "eria"|"eyio";
                  <G4> | <G8> => "aria"|"ayio"; --confirm gendder 8
                  <G5> => "baria" |"ebio";
                  <G3> | <G6> => "ciria"|"ecio";
                  <G7> => "biiraa" |"ebio";
                  <G9> => "aria" |"eyio";
                  <G10> => "ayio"
                   } 
               
           }
    in {
         s = questo ;
        } ;
  there_Adv = mkAdv "vau" ;
  there7to_Adv = mkAdv "vau" ;
  there7from_Adv = mkAdv ["kuma vau"] ;
  therefore_PConj = ss "kwoou" ;
  they_Pron  =mkPron "barabwo" "bo" G1 Pl P3 ;
   
  this_Quant = let
       questo : ParadigmsGus.Number => MorphoGus.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                   <G1> => "oyo";
                   <G2> => "oyo";
                  <G4> => "eri";
                  <G5> => "eke";
                  <G3> => "eye";
                  <G6> => "oro";
                  <G7> => "aka";
                  <G8> => "obo";
                  <G9> => "oko";
                  <G10> => "aa"
                   }; 
                       
      Pl => \\g=> case <g> of{
                  <G1> => "aba";
                  <G2> => "eye";
                  <G4>  => "aya";
                  <G5> |<G7> => "ebi";
                  <G3> |<G6> => "eci";
                  <G8> |<G9>  => " aya";
                  <G10> => "aa"
                   } 
               
           }
    in {
         s = questo ;
        } ;
    through_Prep = mkPrep "kuvitila" ;
  too_AdA = mkAdA "too" ;
  to_Prep = mkPrep "kuvika" ;
  under_Prep = mkPrep "itheo" ;
  very_AdA = mkAdA "muno" ;
 -- want_VV = mkVV (regV "enda") ;
  we_Pron  =mkPron "intwe" "ito" G1 Pl P1 ;
   
  --whatPl_IP = mkIP "ata" "ata"  plural ;
 -- whatSg_IP = mkIP "ata" "ata"  singular ;
  when_IAdv = ss "when" ;
  when_Subj = ss "when" ;
  where_IAdv = ss "where" ;
  which_IQuant = {s = \\_ => "which"} ;
---b  whichPl_IDet = mkDeterminer plural ["which"] ;
---b  whichSg_IDet = mkDeterminer singular ["which"] ;
 -- whoPl_IP = mkIP "uu" "whom" "whose" plural ;
 -- whoSg_IP = mkIP "who" "whom" "whose" singular ;
  why_IAdv = ss "why" ;
  without_Prep = mkPrep "nza" ;
  with_Prep = mkPrep "vamwe na" ;
 --yes_Phr = ss "ii" ;
  yes_Utt = ss "eee" ;
   youSg_Pron  =  mkPron "aye" "o" G1 Sg P2 ;
   
 youPol_Pron,youPl_Pron  = mkPron "inwe" "no" G1 Pl P3 ;
   not_Predet = {s = \\g =>  "nongi"} ;
  --no_Quant =  {s = \\g,n =>  nonExist} ;   
  if_then_Conj = mkConj "ethiwa" "indi" singular ;
  nobody_NP = regNP "obosa" G1 singular ;
  nothing_NP = regNP "obosa" G1 singular ; --confirm gender

  at_least_AdN = mkAdN "at least" ;
  at_most_AdN = mkAdN "at most" ;
  except_Prep = mkPrep "ate o" ;
  as_CAdv = C.mkCAdv "nundu" "ta" ;

 -- have_V2 = dirV2 (mk5V "have" "has" "had" "had" "having") ;
  that_Subj = ss "ati" ;
  lin language_title_Utt = ss "egekusii" ;


}

