concrete StructuralKam of Structural = CatKam ** 
  open MorphoKam, ParadigmsKam, DiffKam,
    (C = ConstructX), Prelude in {

  flags optimize=all ;

  lin
  above_Prep = mkPrep "iulu" False;
  after_Prep = mkPrep "itina wa" False ;
  all_Predet = {s = \\g => MorphoKam.Allpredetprefix g + "onthe"} ;
  almost_AdA = mkAdA "vakuvi " ;
  almost_AdN = mkAdN "vakuvi " ;
  although_Subj = ss "ona kau" ;
  always_AdV = mkAdV "mavinda onthe" ;
  and_Conj = mkConj "na" ;
  because_Subj = ss "nundu" | ss "ni kwithiwa" ;
  before_Prep = mkPrep "mbee" False ;
  behind_Prep = mkPrep "itina" False;
  between_Prep = mkPrep "kati" False;
  both7and_DConj = mkConj "eli" "na";
  but_PConj = ss "ndi" ;
  by8agent_Prep = mkPrep "kwa" False;
  by8means_Prep = mkPrep "na" False ;

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
  during_Prep = mkPrep"ivinda ya" False ;
  either7or_DConj = mkConj "no kethiwa""kana"  singular ;
 everybody_NP = regNP "kila mundu" mu_a singular False ;
  every_Det = { s = \\g =>  "kila"; n=Sg; isPre=  False};
  everything_NP = regNP "undu" u_ma singular False |regNP " kila undu" u_ma singular False;
  everywhere_Adv = mkAdv "kila vandu" ;
  few_Det = mkDet "nini" Pl  True;
  for_Prep = mkPrep nonExist False ;
  from_Prep = mkPrep "kuma" False ;
  he_Pron = mkPron "we" "ake" G1 Sg P3 ;
  here_Adv = mkAdv "vaa" ;
  here7to_Adv = mkAdv "kuvika vaa";
  here7from_Adv = mkAdv "kuma vaa" ;
  how_IAdv = ss "ata" | ss "nzia myau" ;
  how8much_IAdv = ss "mala meana" ;
  how8many_IDet ={ s = table{
                    G4 |G7 |G9  => "syiana";
                    G5  => "twiana";
                    G6  => "kwiana";
                    G2 => "yiana" ;
                    G3 | G1 |G8 |G10  => "meana" 
                    }; n=Pl}; 
     if_Subj = ss "enthwa" ;
  in8front_Prep = mkPrep ["mbee wa"]  False;
 i_Pron   =mkPron "nyie" "akwa" G1 Sg P1 ;
  in_Prep = mkPrep "thini" True ;
 it_Pron   ={ s=\\c=>"yo"; poss=\\n,g=> ""; a=toAgr G4 Sg P3};
       
  less_CAdv = C.mkCAdv "ninangi" "kwi" ;
 much_Det, many_Det = { s =\\g => Manyprefix g + "ingi" ;
               n = Pl;     isPre = True}; 
  more_CAdv = C.mkCAdv "mbee" "wa"; 
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
  no_Phr = ss "aiee" ;
  no_Utt = ss "aiee" ;
  on_Prep = mkPrep "iulu wa" True ;
----  one_Quant = mkDeterminer singular "one" ; -- DEPRECATED
  only_Predet = {s = \\g =>  "tu" } ;
  or_Conj = mkConj "kana" singular ;
  otherwise_PConj = ss "otherwise" ;
  please_Voc = ss "ame" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsKam.Number =>   MorphoKam.Cgender =>  Str = table {
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
    in { s= questo; isFused= False} ;
  quite_Adv = mkAdV "o muno" ;
  she_Pron  = mkPron "we" "ake" G1 Sg P3 ;
           
  so_AdA = mkAdA "so" ;
  somebody_NP = regNP " o mundu " mu_a singular False ;
  someSg_Det = mkDet "mwe" Sg True;
  somePl_Det = mkDet  "mwe" Pl True;
  something_NP = regNP "kindu" ki_i singular False ;
  somewhere_Adv = mkAdv "veo vandu" ;
  that_Quant = let
       questo : ParadigmsKam.Number =>  MorphoKam.Cgender =>  Str = table {
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
  there_Adv = mkAdv "vo" ;
  there7to_Adv = mkAdv "vo" ;
  there7from_Adv = mkAdv ["kuma vau"] ;
  therefore_PConj = ss "Kwoondu wa uu" ;
  they_Pron  =mkPron "mo" "oo" G1 Pl P3 ;
   
  this_Quant = let
       questo : ParadigmsKam.Number =>  MorphoKam.Cgender =>  Str = table {
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
    through_Prep = mkPrep "kwisila" False ;
  too_AdA = mkAdA "too" ;
  to_Prep = mkPrep "nginya" True | mkPrep "kuvika" False;
  under_Prep = mkPrep "uungu wa" False ;
  very_AdA = mkAdA "vyu" ;
 want_VV = MorphoKam.regV "enda";
  we_Pron  =mkPron "ithyi" "itu" G1 Pl P1 ;
   
  whatPl_IP = mkIP "myau"  plural ;
  whatSg_IP = mkIP "kyau"  singular ;
  when_IAdv = ss "indii" ;
  when_Subj = ss "yila" ;
  where_IAdv = ss "kiva" ;
  which_IQuant =  let
       questo : ParadigmsKam.Number =>  DiffKam.Cgender =>  Str =
        table {
    Sg => \\g=> DiffKam.IDetprefixsg g + "va"  ;
    Pl => \\g=> DiffKam.IDetprefixpl g + "va" };
        in { s = questo ; } ;
  whichPl_IDet = {s=\\g=> DiffKam. Cardprefix g + "la"; n=Pl} ;
  whichSg_IDet = {s=\\g=> DiffKam.Cardoneprefix g + "la"  ; n=Sg};
 whoPl_IP = mkIP  "naau" plural ; 
 whoSg_IP = mkIP  "nuu"  singular ;
  why_IAdv = ss "nikii" ;
  without_Prep = mkPrep "nza" False ;
  --with_Prep = mkPrep "vamwe na" ;
  with_Prep =let
       questo : Number =>  MorphoKam.Cgender =>  Str =
        \\n,g =>  DiffKam.Withprefix n g + "na"    
        in { s = questo ; isFused=False} ;
 yes_Phr = ss "ii" ;
  yes_Utt = ss "ii" ;
   youSg_Pron  =  mkPron "we" "aku" G1 Sg P2 ;
   
 youPol_Pron,youPl_Pron  = mkPron "inyui" "enyu" G1 Pl P2 ;
  not_Predet = {s = \\g =>  "nongi"} ;
  no_Quant =  {s = \\g,n =>  nonExist} ;   
  if_then_Conj = mkConj "ethiwa" "indi" singular ;
  nobody_NP = regNP "vai mundu" mu_a singular  False;
  nothing_NP = regNP "vathei" va_ku singular False;

  at_least_AdN = mkAdN "muvaka" ;
  at_most_AdN = mkAdN "nginya" ;

  except_Prep = mkPrep "ate o" False ;

  as_CAdv = C.mkCAdv "nundu" "ta" ;
have_V2 =  dirV2 (iregV "na") ;
 --have_V2 = dirV2 (iregV "na") ;--{s=\\_=>  "na"  []}; --dirV2 (mkV "na") };
  that_Subj = ss "ati" ;
  lin language_title_Utt = ss "kikamba" ;

}

