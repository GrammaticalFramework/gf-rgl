concrete StructuralGus of Structural = CatGus ** 
  open MorphoGus, ParadigmsGus, DiffGus, 
   (C = ConstructX), Prelude in {

  flags optimize=all ;

  lin
  above_Prep = mkPrep "igoro" False ;
  after_Prep = mkPrep "kogwe " False;
  all_Predet = {s = \\g => MorphoGus.Allpredetprefix g + "onsi"} ;
  almost_AdA = mkAdA "ang'e " ;
  almost_AdN = mkAdN " gochia " ;
  although_Subj = ss "nonye" ;
  always_AdV = mkAdV "botambe " ;
  and_Conj = mkConj "na" |mkConj "amo na" ;
  because_Subj = ss "ase  eng’encho" ;
  before_Prep = mkPrep "magega " False ;
  behind_Prep = mkPrep "magega " False;
  between_Prep = mkPrep "gati" False;
  both7and_DConj = mkConj "eli" "na"; --not yet
  but_PConj = ss "korende" ;
  by8agent_Prep = mkPrep "goetera" False ;
  by8means_Prep = mkPrep "kwa" False;---not yet
  during_Prep = mkPrep "gose" False ;
  either7or_DConj = mkConj "gose"  singular ;
 everybody_NP = regNP "kera emunto " G1 singular False ;
 every_Det = { s =\\g => "kera"; n = Sg; isPre =False} ;
   everything_NP = regNP "kera egento" G5 singular False ; --Cgender confirm
  everywhere_Adv = mkAdv "kera ase" ;
  few_Det = {s =\\g => Few_prefix g + "ke" ; n= Pl; isPre=True};
    -- mkDeterminer plural "few" ;
---  first_Ord = ss "first" ; DEPRECATED
  for_Prep = mkPrep "aera" False;
  from_Prep = mkPrep "korwa" False ;
  he_Pron = mkPron "ere" "je" G1 Sg P3 ;
  here_Adv = mkAdv "aa" ;
  here7to_Adv = mkAdv [""] ;--not yet
  here7from_Adv = mkAdv ["igaa"] ;--not yet
  how_IAdv = ss "ing’aki"  ;
  how8much_IAdv = ss "irenga" ;
 how8many_IDet = {s=\\g=> PrefixHowMany g + "renga" ; n=Pl} ;
  if_Subj = ss "onye" ;
  in8front_Prep = mkPrep "bosio" False;
 i_Pron   =mkPron "inche" "ne" G1 Sg P1 ;
  in_Prep =  mkPrep "ase" False | mkPrep "ime" False ;
 it_Pron   ={ s=\\c=>"yo"; poss=\\n,g=> ""; a=Ag G4 Sg P3};-- mkPron "yo" ""G4 Sg P3  ;
     
  less_CAdv = C.mkCAdv "inge" "" ;
   much_Det, many_Det = { s =\\g =>  Many_prefix g + "nge" ; n= Pl; isPre =True };
  more_CAdv = C.mkCAdv "inge" ""; -- more should be a function beccause og Cgender
 -- most_Predet = {s = \\g => MorphoGus.Mostpredetprefix g + "ingi"} ;
   --have_V2 = dirV2 (mkV "bwate") ;
     have_V2 = {
                s=\\_ => [] ;
                s1 =\\ pol,tes,ant,ag =>(subjclitic.s!ag).p1 + "bwate" ;
                imp =\\po,imf => "";
                progV= [];
                 s2 =\\ pol,tes,ant,ag =>[] }** {c2 = noPrep};
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
  on_Prep = mkPrep "igoro" False ;
----  one_Quant = mkDeterminer singular "one" ; -- DEPRECATED
  only_Predet = {s = \\g =>  "yoka" } ;
  or_Conj = mkConj "gose" singular ;
  otherwise_PConj = ss "otherwise" ;
--part_Prep = mkPrep "" ;
  please_Voc = ss "gaki" ;
  part_Prep, possess_Prep = let
       questo : ParadigmsGus.Number =>  MorphoGus.Cgender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G1>| <G2 > => "bwo" ;
                    <G3 > => "ya";
                    <G4 > => "ria";
                    <G5 > => "kia"; --
                    <G6 > => "rwa";
                    <G7> => "ka";
                    <G8> => "bwa";
                    <G9> => "gwa";
                    <G11> => "aa";
                    <G10> => "a"
                   }; 
                       
      Pl => \\g=> case <g> of{
                   <G1> => "ba" ;
                    <G2 > => "ya" ;
                    <G3 >|<G6 > => "chia";
                    <G4 > => "ye" ;
                    <G8>|<G9>|<G10> => "a";
                    <G5 > => "bi"; --
                    <G11> => "aa";
                    <G7> => "bia"
                       }                
           }
    in { s= questo; isFused=False} ;
  quite_Adv = mkAdA "o muno" ;
  she_Pron  = mkPron "ere" "je" G1 Sg P3 ;
           
  so_AdA = mkAdA "igo" ;
  somebody_NP = regNP " omonto gete " G1 singular False;
  someSg_Det = { s =\\g =>  Some_prefix g + "nde"; n= Sg; isPre=True  } ;
  somePl_Det = { s =\\g => Some_prefix g + "nde" ; n= Pl; isPre=True } ;
  something_NP = regNP "egento gete" G1 singular False ; --confirm Cgender
  somewhere_Adv = mkAdv "ase gete" ;
  that_Quant = let
       questo : ParadigmsGus.Number => MorphoGus.Cgender =>  Str = table {
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
                  <G10> |<G11> => "aria"
                   }; 
                       
      Pl => \\g=> case <g> of{
                  <G1> => "baria" ;
                  <G2> => "eria";
                  <G4> | <G8> => "aria"; --confirm gendder 8
                  <G5> => "baria" ;
                  <G3> | <G6> => "ciria";
                  <G7> => "biiraa" ;
                  <G9> => "aria" ;
                  <G11> => "aria" ;
                  <G10> => ""
                   } 
               
           }
    in {
         s = questo ;
        } ;
  there_Adv = mkAdv "abwo" ;
  there7to_Adv = mkAdv "ororo" ;
 -- there7from_Adv = mkAdv ["kuma vau"] ;
  therefore_PConj = ss "ase igo" ;
  they_Pron  =mkPron "barabwo" "bo" G1 Pl P3 ;
   
  this_Quant = let
       questo : ParadigmsGus.Number => MorphoGus.Cgender =>  Str = table {
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
                  <G11> => " aya";
                  <G10> => "aa"
                   }; 
                       
      Pl => \\g=> case <g> of{
                  <G1> => "aba";
                  <G2> => "eye";
                  <G4>  => "aya";
                  <G5> |<G7> => "ebi";
                  <G3> |<G6> => "echi";
                  <G8> |<G9> |<G11> => " aya";
                  <G10> => "aa"
                   } 
               
           }
    in {
         s = questo ;
        } ;
    through_Prep = mkPrep "goetera" False;
  too_AdA = mkAdA "mono" ;
  to_Prep = mkPrep "korwa" False ;
  under_Prep = mkPrep "nyaro" False | mkPrep "inse ye" False;
  very_AdA = mkAdA "mono" ;
 -- want_VV = mkVV (regV "enda") ;
  we_Pron  =mkPron "intwe" "ito" G1 Pl P1 ;
   
  --whatPl_IP = mkIP "ata" "ata"  plural ;
 -- whatSg_IP = mkIP "ata" "ata"  singular ;
  when_IAdv = ss "ekero" ;
  when_Subj = ss "ekero" ;
  where_IAdv = ss "where" ;
    which_IQuant = let
       questo : ParadigmsGus.Number =>  DiffGus.Cgender =>  Str =
        table {
    Sg => \\g=>  "nki"  ;
    Pl => \\g=> "nki" };
        in { s = questo ; } ;
 -- which_IQuant = {s = \\_ => "which"} ;
---b  whichPl_IDet = mkDeterminer plural ["which"] ;
---b  whichSg_IDet = mkDeterminer singular ["which"] ;
  whoPl_IP = mkIP "Nngo"  plural ;
  whoSg_IP = mkIP "Nngo" singular ;
  why_IAdv = ss "why" ;
  without_Prep = mkPrep "" False ;
  with_Prep = mkPrep "babwate"  False;
 --yes_Phr = ss "ii" ;
  yes_Utt = ss "eye" ;
   youSg_Pron  =  mkPron "aye" "o" G1 Sg P2 ;
   
 youPol_Pron,youPl_Pron  = mkPron "inwe" "no" G1 Pl P3 ;
   not_Predet = {s = \\g =>  "nongi"} ;
  --no_Quant =  {s = \\g,n =>  nonExist} ;   
  if_then_Conj = mkConj "ethiwa" "indi" singular ;
  nobody_NP = regNP "obosa" G1 singular False ;
  nothing_NP = regNP "obosa" G1 singular False; --confirm Cgender

  at_least_AdN = mkAdN "inse bi" ;
  at_most_AdN = mkAdN "igoro mono" ;
  except_Prep = mkPrep "otatiga" False ;
  as_CAdv = C.mkCAdv "buna" "";

  --have_V2 = dirV2 (mkV "bwate") ;
  that_Subj = ss "eke" ;
  lin language_title_Utt = ss "egekusii" ;


}

