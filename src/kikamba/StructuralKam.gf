concrete StructuralKam of Structural = CatKam ** 
  open MorphoKam, ParadigmsKam, 
    (C = ConstructX), Prelude in {

  flags optimize=all ;

  lin
  above_Prep = mkPrep "iulu" ;
  after_Prep = mkPrep "itina" ;
  all_Predet = {s = \\g => MorphoKam.Allpredetprefix g + "onthe"} ;
  almost_AdA = mkAdA "vakuvi " ;
  almost_AdN = mkAdN "vakuvi " ;
  although_Subj = ss "ona kau" ;
  always_AdV = mkAdV "mavinda onthe" ;
  and_Conj = mkConj "na" ;
  because_Subj = ss "nundu" | ss "ni kwithiwa" ;
  before_Prep = mkPrep "mbee"  ;
  behind_Prep = mkPrep "itina" ;
  between_Prep = mkPrep "kati" ;
  both7and_DConj = mkConj "eli" "na";
  but_PConj = ss "ndi" ;
  by8agent_Prep = mkPrep "kwa" ;
  by8means_Prep = mkPrep "kwa" ;

{-}  can8know_VV, can_VV = {
    s = table {
      VVF VInf => ["be able to"] ;
      VVF VPres => "can" ;
      VVF VPPart => ["been able to"] ;
      VVF VPresPart => ["being able to"] ;
      VVF VPast => "could" ;      --# notpresent
      VVPastNeg => "couldn't" ;   --# notpresent
      VVPresNeg => "can't" -- | "cannot"     ---- shouldn't be a variant, but replace "can not"
      } ;
    p = [] ;
    typ = VVAux
    } |
 {
    s = table { 
      VVF VInf => ["be able to"] ;
      VVF VPres => "can" ;
      VVF VPPart => ["been able to"] ;
      VVF VPresPart => ["being able to"] ;
      VVF VPast => "could" ;      --# notpresent
      VVPastNeg => "couldn't" ;   --# notpresent
      VVPresNeg => "cannot"     ---- shouldn't be a variant, but replace "can not"
      } ;
    p = [] ;
    typ = VVAux
    } ; -}
  during_Prep = mkPrep "during" ;
  either7or_DConj = mkConj "kana"  singular ;
 everybody_NP = regNP "kila umwe" mu_a singular ;
  every_Det = mkDet "kila" [] Sg ;
  everything_NP = regNP "kila kindu" ki_i singular ;
  everywhere_Adv = mkAdv "kila vandu" ;
  few_Det = mkDet [] "nini" Pl;
  for_Prep = mkPrep nonExist ;
  from_Prep = mkPrep "kuma" ;
  he_Pron = mkPron "we" "ake" G1 Sg P3 ;
  here_Adv = mkAdv "vaa" ;
  here7to_Adv = mkAdv ["kuvika vaa"] ;
  here7from_Adv = mkAdv ["kuma vaa"] ;
  how_IAdv = ss "ata" | ss "nzia myau" ;
  how8much_IAdv = ss "mala meana" ;
 --how8many_IDet = mkDeterminer plural ["mala meana"] ;
  if_Subj = ss "enthwa" ;
  in8front_Prep = mkPrep ["mbee wa"] ;
 i_Pron   =mkPron "nyie" "akwa" G1 Sg P1 ;
  in_Prep = mkPrep "in" ;
 it_Pron   ={ s=\\c=>"yo"; poss=\\n,g=> ""; a=Ag G4 Sg P3};
       
  less_CAdv = C.mkCAdv "ninangi" "kwi" ;
 much_Det, many_Det =mkDet [] "ingi" Pl; 
  more_CAdv = C.mkCAdv "mbeange" "kwi" ; 
  most_Predet = {s = \\g => MorphoKam.Mostpredetprefix g + "ingi"} ;
 
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
  no_Utt = ss "aeee" ;
  on_Prep = mkPrep "iulu wa" ;
----  one_Quant = mkDeterminer singular "one" ; -- DEPRECATED
  only_Predet = {s = \\g =>  "tu" } ;
  or_Conj = mkConj "kana" singular ;
  otherwise_PConj = ss "otherwise" ;
  please_Voc = ss "ame" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsKam.Number =>   MorphoKam.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                      <G2 > |<G9> |<G8 > | <G1 >  => "wa";
                    <G4 > => "kya";
                    <G5 > => "ka";
                    <G6 > => "va";
                    <G10 > => "kwa";
                    _ => "ya"
                   }; 
                       
      Pl => \\g=> case <g> of{
                    <G2 > => "ya";
                    <G5 > => "twa";
                    <G6 > => "kwa";
                    <G7> |<G9>|<G4 > => "sya";
                    _ => "ma"
                   } 
               
           }
    in { s= questo} ;
  quite_Adv = mkAdv "o muno" ;
  she_Pron  = mkPron "we" "ake" G1 Sg P3 ;
           
  so_AdA = mkAdA "so" ;
  somebody_NP = regNP " o mundu " mu_a singular ;
  someSg_Det = mkDet [] "mwe" Sg;
  somePl_Det = mkDet [] "mwe" Pl;
  something_NP = regNP "o kindu" ki_i singular ;
  somewhere_Adv = mkAdv "o vandu" ;
  that_Quant = let
       questo : ParadigmsKam.Number =>  MorphoKam.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G2 > => "iya";
                    <G3 > => "yiya";
                    <G4 > => "kiya";
                    <G5 > => "kaya";
                    <G10 > => "kuya";
                    <G6 > => "vaya";
                    <G1> |<G8> |<G9>  => "uya" ;
                    _ => "iya"
                   }; 
                       
      Pl => \\g=> case <g> of{
                    <G2 > => "iya";
                    <G4 > => "iya";
                    <G5 > => "tuya";
                    <G6 > => "kuya";
                    <G7> |<G9> => "iya";
                    _ => "aya"
                   } 
               
           }
    in {
         s = questo ;
        } ;
  there_Adv = mkAdv "vau" ;
  there7to_Adv = mkAdv "vau" ;
  there7from_Adv = mkAdv ["kuma vau"] ;
  therefore_PConj = ss "kwoou" ;
  they_Pron  =mkPron "mo" "oo" G1 Pl P3 ;
   
  this_Quant = let
       questo : ParadigmsKam.Number =>  MorphoKam.Gender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G3 > => "yii";
                    <G4 > => "kii";
                    <G5 > => "kaa";
                    <G6 > => "vaa";
                    <G7> => "ii";
                    <G10 > => "kuu";
                    _ => "uu"
                   }; 
                       
      Pl => \\g=> case <g> of{
                   <G5 > => "tuu";
                   <G6 > => "kuu";
                   <G2> | <G4> | <G7> | <G9>=> "ii";
                    _ => "aa"
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
  we_Pron  =mkPron "ithyi" "itu" G1 Pl P1 ;
   
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
  yes_Utt = ss "ii" ;
   youSg_Pron  =  mkPron "we" "aku" G1 Sg P2 ;
   
 youPol_Pron,youPl_Pron  = mkPron "inyui" "enyu" G1 Pl P3 ;
  
 

  not_Predet = {s = \\g =>  "nongi"} ;
  no_Quant =  {s = \\g,n =>  nonExist} ;   
  if_then_Conj = mkConj "ethiwa" "indi" singular ;
  nobody_NP = regNP "vai mundu" mu_a singular ;
  nothing_NP = regNP "vathei" va_ku singular ;

  at_least_AdN = mkAdN "muvaka" ;
  at_most_AdN = mkAdN "nginya" ;

  except_Prep = mkPrep "ate o" ;

  as_CAdv = C.mkCAdv "nundu" "ta" ;

 -- have_V2 = dirV2 (mk5V "have" "has" "had" "had" "having") ;
  that_Subj = ss "ati" ;
  lin language_title_Utt = ss "kikamba" ;

}

