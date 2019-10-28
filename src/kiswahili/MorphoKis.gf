--# -path=.:../../prelude

--1 Kiswahili morphology Resource Morphology
--
-- Benson Kituku 2017-2018


resource MorphoKis = CommonBantu ,ResKis 
** open Prelude, Predef 
in {

  flags optimize=all ;
  oper 
  let_s: Str="wacha t";
  lets: Str="wacha ";
  dQue: Str="je,";
  inQue: Str="sijui";
 subjectmarker:Agr-> Str =\ag ->(subjclitic.s!ag).p1;
 form,forms: VForm= VGen;
 --cBind : Str = Predef.BIND ;
 subject: Agr-> Str =\ag -> (subjclitic.s!ag).p1 +   "me" ;
 mkSuffix : Str -> Str = \c -> Predef.BIND ++ c ;
  Many_prefix : Cgender ->  Str = \g ->
   case <g> of {    
   <G1>   => "we" ;
    <G2>   => "mi"  ;
    <G10> => "nyi" ;
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G4>    => "vi" ;
     <G5>|<G6> => "nyi" ;    
    <G7> |<G13>  => "mwi" ;
     <G3>|<G8> |<G9>  => "me"
      } ;


  Few_prefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>   => "wa" ;
    <G2>   => "mi"  ;
    <G10> => "" ;
    <G11> => "pa" ;
    <G12> => "ku" ;
    <G4>    => "vi" ;
     <G5>|<G6> => "" ;    
    <G7> |<G13>  => "m" ;
     <G3>|<G8> |<G9>  => "ma"
      } ;

  Detsomesgprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G3> => "li" ;
    <G4>   => "ki" ;
    <G9>  => "me";
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G5>|<G10> => "nyi" ; 
    <G1> |<G6>|<G2>|<G7>|<G8> |<G13> => "mwi" 

      } ;

Detsomeplprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>   => "we" ;
    <G2>   => "mi"  ;
    <G10> => "nyi" ;
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G4>    => "vi" ;
     <G5>|<G6> => "nye" ;    
    <G7> |<G13>  => "mwi" ;
     <G3>|<G8> |<G9>  => "me"

      } ;

  

 
      mkNum : Str -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two,  second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ two} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ Cardprefix g + two} ; 
       ten  => table {NCard =>\\g =>second ; 
                      NOrd => \\g => Ordprefix g ++ second ++"na" ++ Cardprefix g + two};
       hund  => table {NCard =>\\g =>"mia "  ++ two ;
                        NOrd => \\g => Ordprefix g ++ "mia "  ++ two }
       } } ;
  
   mkNum2 : Str -> Str  -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, twelve,  second ->
    {s = table {
       unit => table {NCard =>\\g => Cardtwoprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ Cardtwoprefix g + two; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ Cardtwoprefix g + two} ; 
       ten  => table {NCard =>\\g =>twelve ; 
                      NOrd => \\g => Ordprefix g ++ twelve};
       hund  => table {NCard =>\\g =>"mia mb "  + two ; 
                      NOrd => \\g => Ordprefix g ++ "mia mb" + two}  
       }} ;

    mkNum1 : Str ->  Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, second -> 
    {s = table {
       unit => table {NCard =>\\g => Cardoneprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ Cardoneprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ Cardoneprefix g + two} ; 
       ten  => table {NCard =>\\g =>"kumi" ; 
                      NOrd => \\g => Ordprefix g ++ "kumi"};
       hund  => table {NCard =>\\g =>"mia "  ++ two ; 
                      NOrd => \\g => Ordprefix g ++ "mia" ++ two}  
       }
    } ;

  regNum : Str ->Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \six,sixth -> {s = table {
       unit => table {NCard =>\\g => six ; 
                      NOrd => \\g => Ordprefix g ++ six} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ six} ; 
       ten  => table {NCard =>\\g =>sixth ; 
                      NOrd => \\g => Ordprefix g ++ sixth ++"na" ++ six };
       hund  => table {NCard =>\\g =>"mia "  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "mia" ++ six}  
       } } ;

 
  regCardOrd : Str -> {s : CardOrd => Cgender => Str} = \ten ->
    {s = table {NCard => \\g =>  ten ; 
		NOrd =>\\g => Ordprefix g ++ ten } } ;  

    regCardone : Str -> Str -> {s : CardOrd => Cgender => Str} = \ten,one ->
    {s = table {NCard => \\g =>  ten ++ Cardoneprefix g + one ; 
    NOrd =>\\g => Ordprefix g ++ ten ++ Cardoneprefix g + one  } } ;

  mkCard : CardOrd -> Str -> Cgender => Str = \o,ten -> 
    (regCardOrd ten).s ! o ; 

   regN : Str ->Cgender -> Noun =  \w, g -> let wpl = case g of {
              G1=>case w of {
                     "mwa" + _  => PrefixPlNom G1  + Predef.drop 3 w ; 
                     "mwi" + _  => "we"  + Predef.drop 3 w ;  
                     "ki" + _  => PrefixPlNom G4  + Predef.drop 2 w ; 
                     "m" + _  => PrefixPlNom G1  + Predef.drop 1 w ;  
                      _   =>  w }; 
               G2=>case w of {
                     "mw" + _  => PrefixPlNom G2  + Predef.drop 2 w ; 
                     "mu" + _  => PrefixPlNom G2 + Predef.drop 2 w ;  
                         _  => PrefixPlNom G2  + Predef.drop 1 w };
                G4=> case w of {
                     "ki" + _  => PrefixPlNom G4  + Predef.drop 2 w ; 
                     "ch" + _  => "vy" + Predef.drop 2 w ;  
                      _   =>  w };
              G6 |G8 => PrefixPlNom g  + Predef.drop 1 w;
              G11 |G12|G13 => "" ;
               _ => PrefixPlNom g + w };
                in mkNoun w wpl g ;
 

    iregN :Str-> Str ->Cgender -> Noun= \man,men,g ->mkNoun man men g;
  mkNoun :Str-> Str ->Cgender -> Noun= \man,men,g ->  { -- for irregular noun
    s = table{Sg => table{Nom => man ; 
                          Loc=> man + "ni"   }; 
              Pl => table{Nom => men ; Loc=> men + "ni" }} ;
    g =  g;
    } ;
regA :Str->{s : AForm =>  Str}= \adj ->regAdj adj ("vi"+adj);
 regAdj:Str -> Str-> {s : AForm =>  Str} = \seo,see ->  {s = table {
     AAdj G1 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"u"  => VowelAdjprefix G1 Sg + seo;
                   _ => ConsonantAdjprefix  G1 Sg + seo };
     AAdj G1 Pl =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G1 Pl + seo;
                  "i"  => VoweliAdjprefix G1 Pl + seo;
                   _ => ConsonantAdjprefix  G1 Pl + seo };

     AAdj G2 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => VowelAdjprefix G2 Sg + seo;
                   _ => ConsonantAdjprefix  G2 Sg + seo };
    AAdj G2  Pl =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G2 Pl + seo;
                  "i"  => VoweliAdjprefix G2 Pl + seo;
                   _ => ConsonantAdjprefix  G2 Pl + seo };
    AAdj G3 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => VowelAdjprefix G3 Sg + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
    AAdj G3  Pl =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G3 Pl + seo;
                  "i"  => VoweliAdjprefix G3 Pl + seo;
                   _ => ConsonantAdjprefix  G3 Pl + seo };

 AAdj G4 n =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G4 n + seo;
                  "i"  => VoweliAdjprefix G4 n + seo;
                   _ => ConsonantAdjprefix  G4 n + seo };
 AAdj G5 n => case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G5 n + seo;
               "i"  => "ny" + Predef.drop 1 seo;
               "d"|"g"|"z"  => "n" +  seo;
               "b"|"p"|"v" => "m" +  seo;
                   _ => ConsonantAdjprefix  G5 n + seo };
   
    AAdj G6  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => VowelAdjprefix G6 Sg + seo;
                   _ => ConsonantAdjprefix  G6 Sg + seo };
    AAdj G6  Pl =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G6 Pl + seo;
               "i"  => "ny" + Predef.drop 1 seo;
               "d"|"g"|"z"  => "n" +  seo;
               "b"|"p"|"v" => "m" +  seo;
                   _ => ConsonantAdjprefix  G6 Pl + seo };

 AAdj G7 n =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G7 n + seo;
                  "i"  => VoweliAdjprefix G7 n + seo;
                   _ => ConsonantAdjprefix  G7 n + seo };
 AAdj G8 n =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G8 n + seo;
                  "i"  => VoweliAdjprefix G8 n + seo;
                   _ => ConsonantAdjprefix  G8 n + seo };
 AAdj G9 n =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G9 n + seo;
                  "i"  => VoweliAdjprefix G9 n + seo;
                   _ => ConsonantAdjprefix  G9 n + seo };
 AAdj G10 n =>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G9 n + seo;
                  "i"  => VoweliAdjprefix G9 n + seo;
                   _ => ConsonantAdjprefix  G9 n + seo };
   
    AAdj G11  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G11 Sg + seo;
                  "i"  => VoweliAdjprefix G11 Sg + seo;
                   _ => ConsonantAdjprefix  G11 Sg + seo };
    
  AAdj G12 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G12 Sg + seo;
                  "i"  => VoweliAdjprefix G12 Sg + seo;
                   _ => ConsonantAdjprefix  G12 Sg + seo };
   AAdj G13 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"o"|"u"  => VowelAdjprefix G13 Sg + seo;
                  "i"  => VoweliAdjprefix G13 Sg + seo;
                   _ => ConsonantAdjprefix  G13 Sg + seo };
    AAdj _  Pl =>[]; 
    Advv => see} };

       


        
iregA : Str ->  {s : AForm =>  Str} =\seo -> {  
       s = table {
            AAdj g n => seo;
            --AAdj g Pl => seoo
            Advv => "vi" ++ seo} };
            

   cregA : Str->  {s : AForm =>  Str} = \seo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + "a rangi ya"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a rangi ya" ++ seo;
             Advv => []} } ;   
  
            

regV :Str -> Verb =\vika -> let  stem = init vika in
mkVerb vika (stem+"i") ("ku"+vika)("hu" + vika ) ;

iregV : Str -> Verb =\vika -> mkVerb vika vika vika vika ;


mkVerb :(gen,preneg,inf,habit : Str) -> Verb= \gen,preneg,inf,habit ->
      { s =table{ 
             VPreNeg   => preneg;
             VGen => gen;
             VInf => inf;
             Vhabitual =>habit;
             VExtension type=> init gen + extension  type
              };
        s1 =\\ pol,tes,ant,ag => let
            v_prefix = (polanttense.s!pol!tes!ant!ag).p1 ; in
            case < tes, ant,pol > of {
              <Pres, Simul, Neg> =>  v_prefix + preneg ;
              <Pres, Simul,Pos> =>  v_prefix + gen;-- | habit;
              <_, _,_> => v_prefix +gen
              };
              progV = [];
         s2=\\pol,tes,ant,ag =>  case < tes ,pol> of {
     <Pres, Neg> =>(polanttense.s!Neg!Pres!Simul! ag).p1 + preneg  ;
     <_, _> =>(polanttense.s!Pos!Pres!Simul! ag).p1 + gen};
     imp=\\po,imf => case <po,imf> of {
                    <Pos,ImpF Sg False> =>  gen;
                    <Pos,ImpF Pl False> =>  case last gen of {
                     "a"  => init gen +"eni";
                      _  =>  gen + "ni"   };
                    <Pos,ImpF Sg True> =>  case last gen of {
                     "a"  => "u" + init gen +"e";
                      _  =>  "u" + gen   };
                    <Pos,ImpF Pl True> =>  case last gen of {
                     "a"  => "m" + init gen +"e";
                      _  =>  "m" + gen   };
                    <Neg, ImpF Sg _> => "usi" + init gen +"e"   ;
                    <Neg,ImpF Pl _> => "msi" + init gen +"e"    }
        
      }; 

regVP run  = { 
      s =\\ ag,pol,tes,ant =>run.s1!pol!tes!ant!ag; 
      compl=\\_=> [];
      progV = run.progV;
      imp=\\po,imf => run.imp!po!imf; 
      inf= run.s!VInf };
 {-}    
 regV :Str -> Verb =\vika -> {
     s =table{ 
             VPreNeg   => init vika +"i";
             VAtense => vika;
             VExtension type=> init vika + extension  type;
             VInf => "ku" +vika;
             Vhabitual =>"hu"+ vika}}; -}
   regVP : Verb -> VerbPhrase ;
  regVP2 : (Verb ** {c2 :  Preposition}) -> SlashVP = \verb ->regVP verb ** {c2 = verb.c2; isFused=verb.isFused} ; 



auxProgBe : VerbPhrase= { s = \\ ag , pol , tense , anter =>
    case < tense ,pol> of {
     <Pres, Neg> => [];
     <Pres, Pos> => [];
     <_, _> => auxBe.s !ag!pol!tense!anter};
      s1=\\_,_,_,_=> []; compl=\\_=> [] ; progV = [];
      imp =\\po,imf => "";inf= ""};
  
   
   
    auxBe : VerbPhrase= { s = \\ ag , pol , tense , anter =>
    case < tense ,pol> of {
     <Pres, Neg> => "si";
     <Pres, Pos> => "ni";
     <Fut, Pos> => (subjclitic.s!ag).p1 + "takuwa";
     <Fut, Neg> => (subjclitic.s!ag).p2 + "takuwa";
     <Past, Neg> =>(subjclitic.s!ag).p2 + "kuwa";
     <Past, Pos> => (subjclitic.s!ag).p1 + "likuwa";
     <Cond, Pos> => (subjclitic.s!ag).p1 + "mekuwa";
     <Cond, Neg> => (subjclitic.s!ag).p2 + "mekuwa" };
      s1=\\_,_,_,_=> []; compl=\\_=> [] ;progV = []; imp =\\po,imf => "";inf= ""};

 polanttense : Poltemp ;
subjclitic: VerbSubjclitic;
polanttense : Poltemp  ={s=\\p,t,a,ag => case <t,a,p> of {
        <Past, Anter, Pos> => < (subjclitic.s!ag).p1 + "likuwa" ++ (subjclitic.s!ag).p1+ "me",[]> ; 
        <Past, Anter, Neg> => <(subjclitic.s!ag).p1+ "likuwa" ++ (subjclitic.s!ag).p2+ "ja" ,[]>; 
        <Past, Simul,Pos> => <(subjclitic.s!ag).p1+ "li",[] >; ---some isues
        <Past, Simul,Neg> => <(subjclitic.s!ag).p2+ "ku",[]> ; -- for "ti" consder oper since some agreement dont take it
        <Pres, Simul,Pos> => <(subjclitic.s!ag).p1+ "na",[] >; ---done
        <Pres, Simul,Neg> => <(subjclitic.s!ag).p2  ,[] >; 
        <Pres, Anter,Pos> => <(subjclitic.s!ag).p1+ "me",[]> ;
        <Pres, Anter,Neg> => <(subjclitic.s!ag).p2+ "ja",[]> ;
        <Fut, Simul,Pos> => <(subjclitic.s!ag).p1+ "ta" ,[]> ; ---done
        <Fut, Simul,Neg> => <(subjclitic.s!ag).p2+ "ta",[]>; 
        <Fut, Anter,Pos> => <(subjclitic.s!ag).p1+ "takuwa" ++ (subjclitic.s!ag).p1+ "me" ,[]>;
        <Fut, Anter,Neg> => <(subjclitic.s!ag).p2+ "takuwa" ++ (subjclitic.s!ag).p1+ "me",[] > ;
        <Cond, Simul,Pos> => <(subjclitic.s!ag).p1+ "nge" ,[]> ;---done 
        <Cond, Anter,Neg> => <(subjclitic.s!ag).p2+ "ngeli",[]>  ;
        <Cond, Anter,Pos> => <(subjclitic.s!ag).p1+ "ngeli",[]> ;
        <Cond, Simul,Neg> => <(subjclitic.s!ag).p2+ "ngali",[] >
      }};
Poltemp : Type ={ s: Polarity => Tense => Anteriority =>  Agr => Str *Str}; 

VerbSubjclitic : Type = {s : Agr => Str * Str };
subjclitic : VerbSubjclitic = { s=\\a => case a of {
            Ag G1 Sg P1 =><"ni","si">;
            Ag G1 Sg P2 => <"u","hu">;
            Ag G1 Sg P3 => <"a","ha">;
            Ag G1 Pl P1 =><"tu","hatu">;
            Ag G1 Pl P2 => <"m","ham">;
            Ag G1 Pl P3 => <"wa","hawa">;
            Ag G2 Sg P3=><"u","hau">;
            Ag G2 Pl P3 =><"i","hai">;
            Ag G3 Sg P3 =><"li","hali">;
            Ag G3 Pl P3 =><"ya","haya">;
            Ag G4 Sg P3 => <"ki","haki">;
            Ag G4 Pl P3 => <"vi","havi">;
            Ag G5 Sg P3 => <"i","hai">;
            Ag G5 Pl P3 => <"zi","hazi">;
            Ag G6 Sg P3 => <"u","hau">;
            Ag G6 Pl P3 =><"zi","hazi">;
            Ag G7 Sg P3 => <"u","hau">;
            Ag G7 Pl P3 =><"u","hau">;
            Ag G8 Sg P3 =><"u","hau">;
            Ag G8 Pl P3 =><"ya","haya">;
            Ag G9 Sg P3 =><"ya","haya">;
            Ag G9 Pl P3 => <"ya","haya">;
            Ag G10 Sg P3 => <"i","hai">;
            Ag G10 Pl P3=><"i","hai">;
            Ag G11 Sg P3 =><"ku","haku">;
            Ag G11 Pl P3 =><"ku","haku">;
            Ag G12 Sg P3 =><"pa","hapa">;
            Ag G12 Pl P3 => <"pa","hapa">;
            Ag G13 Sg P3 => <"m","ham">;
            Ag G13 Pl P3=><"m","ham">;
            Ag  _  _  _ =><"",""> }};
           mkClitic  : Str -> Str = \c -> c ++ Predef.BIND ;
}

