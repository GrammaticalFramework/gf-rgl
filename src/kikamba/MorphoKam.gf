--# -path=.:../../prelude

--Kikamba Resource Morphology
--
-- Benson Kituku 2017 -- 2018

resource MorphoKam =  CommonBantu ,ResKam
 ** open Prelude, Predef,CatKam
in {

  flags optimize=all ;

 oper
 let_s: Str="eka";
  lets: Str="eka ";
 dQue: Str="";
inQue: Str="ndwisi kana"; 
 kuna: Str="ve";
form: VForm= VGen;
forms: VForm= VPast;
subjectmarker:Agr-> Str =\ag ->(presverbsubjclitic.s!ag).p1;
subject: Agr-> Str =\ag -> (pastverbsubjclictic.s!ag).p3;
 mkDet: (mine : Str) -> Number ->Bool -> {s : Cgender =>  Str ; n : Number; isPre: Bool} = 
    \det,num, bool -> 
            { s =\\g => Detsomeplprefix g + det ;
               n = num;
              isPre = bool} ; 

 Detsomesgprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> |<G8> |<G9> => "u" ;
     <G3>   => "yi" ;
    <G4>   => "ki" ;
    <G5> => "ka" ;
    <G10> => "ku" ;
    <G6> => "va" ;
    <G7>   => "i" 
      } ;

Detsomeplprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  => "a" ;
     <G2>   => "i" ;
    <G4>   => "i" ;
    <G5> => "tu" ;
    <G6> => "ku" ;
    <G3> |<G8> |<G10>=> "ma";
    <G7> | <G9>  => "i"  } ;

    Manyprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  => "a" ;
     <G2>   => "mi" ;
    <G7> |<G4> | <G9>   => "mbi" ;
    <G5> => "twi" ;
    <G6> => "kwi" ;
    <G3> |<G8> |<G10>=> "ma" } ;

 mkNum : Str -> Str -> Str -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, twelve, twenty, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikumi na"  ++ twelve ; 
                      NOrd => \\g => Ordprefix g ++ "ikumi na" ++ twelve} ; 
       ten  => table {NCard =>\\g =>"miongo "  ++ twelve ; 
                      NOrd => \\g => Ordprefix g ++ "miongo" ++ twelve};
       hund  => table {NCard =>\\g =>"maana "  ++ twenty ; 
                      NOrd => \\g => Ordprefix g ++ "maana" ++ twenty}  
       }
    } ;
  
   mkNum2 : Str -> Str -> Str -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, twelve, twenty, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardtwoprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikumi na"  ++ twelve ; 
                      NOrd => \\g => Ordprefix g ++ "ikumi na" ++ twelve} ; 
       ten  => table {NCard =>\\g =>"miongo "  ++ twelve ; 
                      NOrd => \\g => Ordprefix g ++ "miongo" ++ twelve};
       hund  => table {NCard =>\\g =>"maana "  ++ twenty ; 
                      NOrd => \\g => Ordprefix g ++ "maana" ++ twenty}  
       }
    } ;

    mkNum1 : Str ->  Str -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, twenty, second -> 
    {s = table {
       unit => table {NCard =>\\g => Cardoneprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikumi na"  ++ Cardoneprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "ikumi na" ++ Cardoneprefix g + two} ; 
       ten  => table {NCard =>\\g =>"ikumi" ; 
                      NOrd => \\g => Ordprefix g ++ "ikumi"};
       hund  => table {NCard =>\\g =>"yiana "  ++ twenty ; 
                      NOrd => \\g => Ordprefix g ++ "yiana" ++ twenty}  
       }
    } ;

  regNum : Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \six -> {s = table {
       unit => table {NCard =>\\g => six ; 
                      NOrd => \\g => Ordprefix g ++ six} ; 
       teen => table {NCard =>\\g =>"ikumi na"  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "ikumi na" ++ six} ; 
       ten  => table {NCard =>\\g =>"miongo "  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "miongo" ++ six};
       hund  => table {NCard =>\\g =>"maana "  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "maana" ++ six}  
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
              G1=>case w of {"mwa" + _  =>  Predef.drop 2 w ; 
                     "mwi" + _  => "e"  + Predef.drop 3 w ;  
                      _  => PrefixPlNom G1  + Predef.drop 2 w }; 
              G2=>case w of {"mw" + _  => "my"  + Predef.drop 2 w ; 
                      _  => PrefixPlNom G2  + Predef.drop 2 w };
              G3 => PrefixPlNom G3  + Predef.drop 1 w;
              G5 => case w of {"ka" + _  => "twa" + Predef.drop 2 w ;  
                      _   =>  PrefixPlNom G4  + Predef.drop 2 w };     
              G4=> case w of {"ky" + _  => "sy" + Predef.drop 2 w ;  
                      _   =>  PrefixPlNom G4  + Predef.drop 2 w };
              G7 =>  w; 
              G8 |G9 => PrefixPlNom g  +  w;
              _ => PrefixPlNom g  + Predef.drop 2 w};                   
          in mkNoun w wpl g ;
 iregN :Str-> Str ->Cgender -> Noun= \man,men,g ->mkNoun man men g;
  mkNoun :Str-> Str ->Cgender -> Noun= \man,men,g ->  { -- for irregular noun
    s = table{Sg => table{Nom => man ; 
                          Loc=> man + "ni"   }; 
              Pl => table{Nom => men ; Loc=> men + "ni" }} ;
    g =  g;
    } ;
 regA :Str->{s : AForm =>  Str}= \adj ->regAdj adj [];
  regAdj:Str -> Str-> {s : AForm =>  Str} = \seo,see ->  {s = table {
     AAdj G1  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"   => "mw" + seo;
               --"n"  => "mwa" + seo;
               "u"  => "m" + seo;
                   _ => ConsonantAdjprefix  G1 Sg + seo };
      AAdj G1 Pl =>case Predef.take 1 seo of { 
                           "u"  => "o" + Predef.drop 1 seo;
                   _ => ConsonantAdjprefix  G1 Pl + seo };

     AAdj G2 Sg=>case Predef.take 1 seo of { 
               "i"  => "mw" + seo;
               "a"  => "my" + seo;
               "u"  => "m" + seo;
                   _ => ConsonantAdjprefix  G2 Sg + seo };
     AAdj G2  Pl =>case Predef.take 1 seo of { 
               "u"  => "my" + seo;
                  "i"  => "m" + seo;
                   _ => ConsonantAdjprefix  G2 Pl + seo };
  
      AAdj G3 Sg=>case Predef.take 1 seo of { 
               "i"|"u"|"e"  => "y" + seo;
                  "a"  => "yi" + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
      AAdj G3 Pl =>case Predef.take 1 seo of { 
                "e" => "m"+  seo;
               "u"  => "mo"+ Predef.drop 1 seo;
                     _ => ConsonantAdjprefix  G3 Pl + seo };

  
      AAdj G4 Sg=>case Predef.take 1 seo of { 
               "i"  => "k" + seo;
                  "u" |"a"  => "ky" + seo;
                   _ => ConsonantAdjprefix  G4 Sg + seo };
      AAdj G4 Pl =>case Predef.take 1 seo of { 
               "u"  => "mb"+  seo;
                "i"  => "nz" + seo;
               "a"  => "nd" + seo;
              --- "t" => "nd" +  Predef.drop 1 seo; consider thuku
               "s" => "nz" +  Predef.drop 1 seo;
               _ => ConsonantAdjprefix  G4 Pl + seo };
     AAdj G5 Sg=>case Predef.take 1 seo of { 
                 "u"  => "ko" + Predef.drop 1  seo;
                   _ => ConsonantAdjprefix  G5 Sg + seo };
     AAdj G5  Pl =>case Predef.take 1 seo of { 
              "u"  => "t"+ seo;
               "a" | "e"| "i"  => "tw" + seo;
                _ => ConsonantAdjprefix  G5 Pl + seo };

      AAdj G6 Sg=>case Predef.take 1 seo of { 
               "u"  => "vo" + Predef.drop 1 seo;
                   _ => ConsonantAdjprefix  G6 Sg + seo };
      AAdj G6 Pl =>case Predef.take 1 seo of { 
               "u"  => "k" + seo;
               "i"|"a"  => "kw" +  seo;
               _ => ConsonantAdjprefix  G6 Pl + seo };

     AAdj G7 n =>case Predef.take 1 seo of { 
               "s" => "nz" +  Predef.drop 1 seo;
               "i"  => "nz" +  seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + Predef.drop 1 seo;
                  "t" | "a"  => "nd" +  Predef.drop 1 seo;
                 _ => ConsonantAdjprefix  G7 n + seo };

      AAdj G9 Pl  =>case Predef.take 1 seo of { 
               "s" |"i"  => "nz" + Predef.drop 1 seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nda" +   seo;
                 _ => ConsonantAdjprefix  G9 Pl + seo };
     AAdj G10 Sg =>case Predef.take 1 seo of { 
               "u"  => "k" + seo;
               "i"|"a"  => "kw" +  seo;
               _ => ConsonantAdjprefix  G10 Sg + seo };

      AAdj g Pl =>case Predef.take 1 seo of { 
               "u"  => "mo" + Predef.drop 1 seo;
               _ => ConsonantAdjprefix  g Pl + seo };
       AAdj g Sg=>case Predef.take 1 seo of { 
                "i"  => "mw" + seo;
                "a"  => "my" + seo;
               "u"  => "m" +  seo;
                   _ => ConsonantAdjprefix  g Sg + seo };
     AComp G1 Sg=>let af : Str = case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"  => "mw" + seo;
               "u"  => "m" + seo;
                   _ => ConsonantAdjprefix  G1 Sg + seo };
                   in init af + "ang" + last af;
      AComp G1 Pl =>let af : Str = case Predef.take 1 seo of { 
                           "u"  => "o" + Predef.drop 1 seo;
                   _ => ConsonantAdjprefix  G1 Pl + seo }
                 in init af + "ang" + last af;

  
     AComp G2 Sg=>let af : Str = case Predef.take 1 seo of { 
               "i"  => "mw" + seo;
               "a"  => "my" + seo;
               "u"  => "m" + seo;
                   _ => ConsonantAdjprefix  G2 Sg + seo };
                   in init af + "ang" + last af;
     AComp G2 Pl =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "my" + seo;
                  "i"  => "m" + seo;
                   _ => ConsonantAdjprefix  G2 Pl + seo }
                 in init af + "ang" + last af;
  
     AComp G3 Sg=>let af : Str = case Predef.take 1 seo of { 
               "i"|"u"  => "y" + seo;
                  "a"  => "yi" + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
                   in init af + "ang" + last af;
      AComp G3 Pl =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "mo"+ Predef.drop 1 seo;
               _ => ConsonantAdjprefix  G3 Pl + seo }
             in init af + "ang" + last af;

        AComp G4 Sg=>let af : Str = case Predef.take 1 seo of { 
               "i"  => "k" + seo;
                  "u"  => "ky" + seo;
                   _ => ConsonantAdjprefix  G4 Sg + seo };
                   in init af + "ang" + last af;
      AComp G4 Pl =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "mb"+  seo;
                "i"  => "sy" + seo;
               "a"  => "nd" + seo;
               _ => ConsonantAdjprefix  G4 Pl + seo }
             in init af + "ang" + last af;
   AComp G5 Sg=>let af : Str = case Predef.take 1 seo of { 
                 "u"  => "ko" + Predef.drop 1  seo;
                   _ => ConsonantAdjprefix  G5 Sg + seo };
                   in init af + "ang" + last af;
      AComp G5 Pl =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "t"+ seo;
               "a" | "i"  => "tw" + seo;
                _ => ConsonantAdjprefix  G5 Pl + seo }
              in init af + "ang" + last af;

     AComp G6 Sg=>let af : Str = case Predef.take 1 seo of { 
               "u"  => "vo" + Predef.drop 1 seo;
                   _ => ConsonantAdjprefix  G6 Sg + seo };
                   in init af + "ang" + last af;
     AComp G6 Pl =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "k" + seo;
               "i"|"a"  => "kw" +  seo;
               _ => ConsonantAdjprefix  G6 Pl + seo }
             in init af + "ang" + last af;

 AComp G7 n =>let af : Str = case Predef.take 1 seo of { 
               "s" |"i"  => "nz" + Predef.drop 1 seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nd" + seo;
                 _ => ConsonantAdjprefix  G7 n + seo };
                 in init af + "ang" + last af;

AComp G9 Pl  =>let af : Str = case Predef.take 1 seo of { 
               "s" |"i"  => "nz" + Predef.drop 1 seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nd" + seo;
                 _ => ConsonantAdjprefix  G9 Pl + seo };
                 in init af + "ang" + last af;
  AComp G10 Sg =>let af : Str = case Predef.take 1 seo of { 
               "u"  => "k" + seo;
               "i"|"a"  => "kw" +  seo;
               _ => ConsonantAdjprefix  G10 Sg + seo }
             in init af + "ang" + last af;
  AComp  g Pl => let af : Str = case Predef.take 1 seo of { 
               "u"  => "mo" + Predef.drop 1 seo;
               _ => ConsonantAdjprefix  g Pl + seo }
             in init af + "ang" + last af;

  AComp  g Sg=>let af : Str = case Predef.take 1 seo of { 
                "i"  => "mw" + seo;
                "a"  => "my" + seo;
               "u"  => "m" +  seo;
                   _ => ConsonantAdjprefix  g Sg + seo };
                   in init af + "ang" + last af;
   Advv => see
  }   };             


                      
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
             AAdj g Sg =>  seo; 
             AAdj g Pl=> seoo ;
            AComp g Sg =>  init  seo + "ang" + last seo;
            AComp g Pl => init  seoo + "ang" + last seoo;
            Advv =>[]}                          
       }  ;

--regular expression for adjective colours
 cregA : Str-> {s : AForm =>  Str} = \seo -> {s = table {
             AAdj g Sg=> ProunSgprefix g + "a langi wa"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a langi wa"  ++ seo;
           AComp g n =>  [];
           Advv =>[]}} ;
sregA : Str-> {s : AForm =>  Str} = \seo -> {s = table {
             AAdj g Sg => ProunSgprefix g + "a"   ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a"   ++ seo;
            AComp g n =>  nonExist;
            Advv =>nonExist} } ;

   

    
vprogressive : Str -> Str = \root ->
    case Predef.dp 1 root of {
      "b" |"v"|"m"  => root +  "ete";
           _   => root + "ite" } ;   

regV :Str -> Verb =\vika -> let  stem = init vika in
mkVerb vika (vprogressive stem) ("ku"+vika)(stem + "ie")(stem+"aa") ;

iregV : Str -> Verb =\vika -> mkVerb vika vika vika vika vika;


mkVerb :(gen,prog,inf,past,predef : Str) -> Verb= \gen,prog,inf,past,predef ->
      { s =table{ 
              VGen =>gen;
              VPreProg => prog;
              VInf => inf;
              VPast =>  past;
              VPreDef =>predef;
              VInf => inf;
              VExtension type=> init gen + extension  type + last gen
              };
        s1 =\\ pol,tes,ant,ag => let
            v_prefix = (polanttense.s!pol!tes!ant!ag).p1 ; in
            case < tes, ant,pol > of {
              <Pres, Simul, _> =>  v_prefix + predef ;
              <Cond, Simul,Pos> | <Past, Simul,Pos> => v_prefix+ past  ;
              <Past, Anter,_> =>  v_prefix+ prog ; 
              <_, _,_> => v_prefix+ gen};
        progV = prog;
        imp=\\po,imf => case <po,imf> of {
                    <Pos,ImpF Sg _> =>  gen;
                    <Pos,ImpF Pl _> =>  gen + "i";
                     <Neg, _> => ""       } }; 


regVP run  = { 
      s =\\ ag,pol,tes,ant =>run.s1!pol!tes!ant!ag; 
      compl=\\_=> [];
      progV = run.progV;
      imp=\\po,imf => run.imp!po!imf; 
      inf= run.s!VInf };

--oper getNum : ImpForm->  Number = \gn ->
 --  case gn of { ImpF Sg _ => Sg ; _ => Pl } ;
regVP : Verb -> VerbPhrase ;
 regVP2 : (Verb ** {c2 :  Preposition}) -> SlashVP = \verb -> regVP verb ** {c2 = verb.c2 ; isFused=verb.c2.isFused} ;


 
predVc : (Verb ** {c2 : Preposition}) -> SlashVP = \verb ->
      regVP verb ** {c2 = verb.c2} ;
auxProgBe : VerbPhrase= { s = \\ ag , pol , tense , anter =>
    case < tense ,pol> of {
     <Pres, Neg> =>"ndi";
     <Pres, Pos> => "ni";
     <_, _> => auxBe.s !ag!pol!tense!anter};
      compl=\\_=> [] ;imp =\\po,imf => ""; progV=[];inf= ""};
      
       auxBe : VerbPhrase= { s = \\ agr , pol , tense , anter =>
    case < tense ,anter, pol, agr > of {
     <Pres,_, Pos, Ag G1 Pl P1> => "twi";
    -- <Pres, Pos, Ag G1 Sg P3> => "wi";
     <Pres,_, Pos, Ag _ _ _> => "ni";
     <Pres,_, Neg, Ag G1 Sg P1> => "ndi";
     <Pres,_, Neg, Ag G1 Sg _> => "ndwi";
     <Pres,_, Neg, Ag _ _ _> => "ti";
     <Past,_,Pos,Ag _ _ _> =>(auxillary.s!agr).p5  ;
     <Fut, Anter,Pos , Ag _ _ _> => (auxillary.s!agr).p1  ;
     <Fut, Anter,Neg, Ag _ _ _> => (auxillary.s!agr).p1 ;
     <Cond, Simul,Pos , Ag _ _ _> => (auxillary.s!agr).p1 ;
     <Cond, Anter,Pos , Ag _ _ _> => (auxillary.s!agr).p3  ;
     <Cond, Anter,Neg , Ag _ _ _> => (auxillary.s!agr).p4  ;
      <Cond, Simul,Neg, Ag _ _ _> =>  (auxillary.s!agr).p2;
     <_, _, _> => " ni"}; 
         compl=\\_=> [];
      progV=[];
     imp =\\po,imf => "";inf= ""};
oper
futverbsubjclitic: VerbSubjclitic;
 presverbsubjclitic, pastverbsubjclictic : VerbSubjclitic;
  auxillary : VerbAux;
   VerbSubjclitic : Type = {s : Agr => Str*Str*Str*Str  };
   VerbAux : Type = {s : Agr => Str * Str *Str*Str *Str};
pastverbsubjclictic : VerbSubjclitic = { s=\\a => case a of {
                        -- simpos         simneg        anterpos    anterneg
            Ag G1 Sg P1 =><"ni","ndi","na","ndy">;
            Ag G1 Sg P2 => <"u","ndu","wa","ndw">;
            Ag G1 Sg P3 => <"u","ndu","wa","ndw">;
            Ag G1 Pl P1 =><"tu","tu","twa","tuy">;
            Ag G1 Pl P2 => <"mu","mu","mwa","muy">;
            Ag G1 Pl P3 => <"ma","ma","ma","may">;
            Ag G2 Sg P3=><"u","ndu","wa","ndw">;
            Ag G2 Pl P3 =><"-","ndi","ya","ndy">;
            Ag G3 Sg P3 =><"yi","yi","ya","yi">;
            Ag G3 Pl P3 =><"ma","ma","ma","may">;
            Ag G4 Sg P3 => <"ki","ki","kya","kiy">;
            Ag G4 Pl P3 => <"-","i","sya","iy">;
            Ag G5 Sg P3 => <"ka","ka","kaa","kay">;
            Ag G5 Pl P3 => <"tu","tu","twa","tuy">;
            Ag G6 Sg P3 => <"va","va","va","vay">;
            Ag G6 Pl P3 =><"ku","ku","kwa","kuy">;
            Ag G7 Sg P3 => <"-","nde","ya","ndy">;
            Ag G7 Pl P3 =><"-","i","sya","iy">;
            Ag G8 Sg P3 =><"u","nde","wa","ndw">;
            Ag G8 Pl P3 =><"ma","ma","ma","may">;
            Ag G9 Sg P3 =><"u","nde","wa","ndw">;
            Ag G9 Pl P3 => <"-","i","sya","iy">;
            Ag G10 Sg P3 => <"ku","ku","kwa","kuy">;
            Ag G10 Pl P3=><"ma","ma","ma","may">;
            Ag _  _  _  =><"","","",""> }};
       presverbsubjclitic : VerbSubjclitic = { s=\\a => case a of {
              Ag G1 Sg P1 =><"ni","ndi","na","ndi">;
            Ag G1 Sg P2 => <"nu","ndu","wa","ndu">;
            Ag G1 Sg P3 => <"nu","nda","wa","nda">;
            Ag G1 Pl P1 =><"tu","tui","twa","tui">;
            Ag G1 Pl P2 => <"mu","mui","mwa","mui">;
            Ag G1 Pl P3 => <"ma","tia","ma","mai">;
            Ag G2 Sg P3=><"u","ndu","wa","ndu">;
            Ag G2 Pl P3 =><"i","ndi","ya","ndi">;
            Ag G3 Sg P3 =><"yi","yii","ya","yi">;
            Ag G3 Pl P3 =><"ma","mai","ma","mai">;
            Ag G4 Sg P3 => <"ki","kii","kya","ki">;
            Ag G4 Pl P3 => <"i","ii","sya","ii">;
            Ag G5 Sg P3 => <"ka","kai","ka","ka">;
            Ag G5 Pl P3 => <"tu","tui","twa","tui">;
            Ag G6 Sg P3 => <"va","vai","va","vai">;
            Ag G6 Pl P3 =><"ku","kui","kwa","kui">;
            Ag G7 Sg P3 => <"i","ndi","ya","ndi">;
            Ag G7 Pl P3 =><"i","syi","sya","syi">;
            Ag G8 Sg P3 =><"u","ndu","wa","ndu">;
            Ag G8 Pl P3 =><"ma","mai","ma","mai">;
            Ag G9 Sg P3 =><"u","ndu","wa","ndu">;
            Ag G9 Pl P3 => <"i","syi","ma","syi">;
            Ag G10 Sg P3 => <"ku","kui","kwa","kui">;
            Ag G10 Pl P3=><"ma","mai","ma","mai">;
           Ag _  _  _  =><"","","",""> }};
  
-- : Str -> Str = \c -> c ++ Predef.BIND ;
auxillary : VerbAux  = { s=\\a => case a of {
          Ag G1 Sg P1 =><"ngeethiwa","ndikeethiwa","ninesaa","ndeesaa" , "nai">;
            Ag G1 Sg P2 => <"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa","wai">;
            Ag G1 Sg P3 => <"ukeethiwa","ndukeethiwa","niwesaa","ndesaa","wai">;
            Ag G1 Pl P1 =><"tukeethiwa","tutikeethiwa","nitwesaa","tuyesaa", "twai">;
            Ag G1 Pl P2 => <"mukeethiwa","muikeethiwa","nimwesaa","muyesaa","mwai">;
            Ag G1 Pl P3 => <"makeethiwa","maikeethiwa","nimesaa","mayesaa","mai">;
            Ag G2 Sg P3=><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa","wai">;
            Ag G2 Pl P3 =><"ikeethiwa","ndikeethiwa","niyesaa","ndyesaa", "yai">;
            Ag G3 Sg P3 =><"ikeethiwa","yiikeethiwa","niyesaa","iyesaa", "yai">;
            Ag G3 Pl P3 =><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">;
            Ag G4 Sg P3 => <"kikeethiwa","kiikeethiwa","nikyesaa","kiyesaa", "kyai">;
            Ag G4 Pl P3 => <"ikeethiwa","iikeethiwa","nisyesaa","iyesaa", "syai">;
            Ag G5 Sg P3 => <"Kakeethiwa","Kaikeethiwa","nikesaa","kayesaa", "kai">;
            Ag G5 Pl P3 => <"Tukeethiwa","Tuikeethiwa","nitwesaa","tuyesaa", "twai">;
            Ag G6 Sg P3 => <"vakeethiwa","vaikeethiwa","nivesaa","vayesaa", "vai">;
            Ag G6 Pl P3 =><"kukeethiwa","kuikeethiwa","nikwesaa","kuyesaa", "kwai">;
            Ag G7 Sg P3 => <"ikeethiwa","ndikeethiwa","niyesaa","ndeesaa", "yai">;
            Ag G7 Pl P3 =><"syikeethiwa","syikeethiwa","nisyesaa","iyesaa", "syai">;
            Ag G8 Sg P3 =><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa", "wai">;
            Ag G8 Pl P3 =><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">;
            Ag G9 Sg P3 =><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa", "wai">;
            Ag G9 Pl P3 => <"syikeethiwa","syikeethiwa","nisyesaa","iyesaa", "syai">;
            Ag G10 Sg P3 => <"kukeethiwa","kuikeethiwa","nikwesaa","kuyesaa", "kwai">;
            Ag G10 Pl P3=><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">;
             Ag _  _  _  =><"","","","",""> }};


futverbsubjclitic  : VerbSubjclitic = { s=\\a => case a of {
            Ag G1 Sg P1 =><"ni","ndi","ni","nda">;
            Ag G1 Sg P2 => <"u","ndu","u","uta">;
            Ag G1 Sg P3 => <"a","ndu","u","uta">;
            Ag G1 Pl P1 =><"tu","tuti","tu","tuta">;
            Ag G1 Pl P2 => <"mu","muti","mu","muta">;
            Ag G1 Pl P3 => <"ma","mati","ma","mata">;
            Ag G2 Sg P3=><"u","ndu","u","uta">;
            Ag G2 Pl P3 =><"i","ndi","i","ita">;
            Ag G3 Sg P3 =><"i","ii","yi","yita">;
            Ag G3 Pl P3 =><"ma","mai","ma","mata">;
            Ag G4 Sg P3 => <"ki","kii","ki","kita">;
            Ag G4 Pl P3 => <"i","ii","ii","syita">;
            Ag G5 Sg P3 => <"ka","kai","ka","kata">;
            Ag G5 Pl P3 => <"tu","tui","tu","tuta">;
            Ag G6 Sg P3 => <"va","vai","va","vata">;
            Ag G6 Pl P3 =><"ku","kui","ku","kuta">;
            Ag G7 Sg P3 => <"i","ii","yi","yita">;
            Ag G7 Pl P3 =><"i","syii","syi","syita">;
            Ag G8 Sg P3 =><"u","ndu","u","uta">;
            Ag G8 Pl P3 =><"ma","mai","ma","mata">;
            Ag G9 Sg P3 =><"u","ndu","u","uta">;
            Ag G9 Pl P3 => <"i","syii","syi","syita">;
            Ag G10 Sg P3 => <"ku","kui","ku","kuta">;
            Ag G10 Pl P3=><"ma","mai","ma","mata">;
           Ag _  _  _  =><nonExist,nonExist,nonExist,nonExist>}};

mkSuffix : Str -> Str = \c -> Predef.BIND ++ c ;



polanttense : Poltemp ;
Poltemp : Type ={ s: Polarity => Tense => Anteriority =>  Agr => Str*Str};
 polanttense : Poltemp  ={s=\\p,t,a,ag => case <t,a,p> of {
        <Past, Anter, Pos> => < "ni"+ (pastverbsubjclictic.s!ag).p3 ,mkSuffix"ete">; 
        <Past, Anter, Neg> => <(pastverbsubjclictic.s!ag).p4  + "a",mkSuffix"ete">; 
        <Past, Simul,Pos> => <"ni"+ (pastverbsubjclictic.s!ag).p1  + "na",mkSuffix"ie">; ---some isues
        <Past, Simul,Neg> =>< (pastverbsubjclictic.s!ag).p2  + "ti" + "nee",mkSuffix"a">; -- for "ti" consder oper since some agreement dont take it
        <Pres, Simul,Pos> =>< "ni"+ (presverbsubjclitic.s!ag).p1,mkSuffix"aa">; ---done
        <Pres, Simul,Neg> =>< (presverbsubjclitic.s!ag).p2,"aa" ,mkSuffix"a" > ; 
        <Pres, Anter,Pos> =>< "ni"+ (presverbsubjclitic.s!ag).p3,mkSuffix"a"> ;
        <Pres, Anter,Neg> => <(presverbsubjclitic.s!ag).p4  + "na",mkSuffix"a"> ;
        <Fut, Simul,Pos> => <(futverbsubjclitic.s!ag).p1 + "ka" ,mkSuffix"a"> ; ---done
        <Fut, Simul,Neg> => <(futverbsubjclitic.s!ag).p2 + "ka",mkSuffix"a">; 
        <Fut, Anter,Pos> => <auxBe.s!ag!Pos!Fut!Anter ++(futverbsubjclitic.s!ag).p3 + "na",mkSuffix"a"> ;
        <Fut, Anter,Neg> => <auxBe.s!ag!Neg!Fut!Anter ++ (futverbsubjclitic.s!ag).p4 + "na" ,mkSuffix"a">;
        <Cond, Simul,Pos> => <auxBe.s!ag!Pos!Cond!Simul ++(pastverbsubjclictic.s!ag).p3,"ie" ,mkSuffix"a">;---done 
        <Cond, Anter,Pos> => <auxBe.s!ag!Pos!Cond!Anter + "ku",mkSuffix"a"> ;
        <Cond, Anter,Neg> => <auxBe.s!ag!Neg!Cond!Anter + "ku",mkSuffix"a"> ;
        <Cond, Simul,Neg> =>  <auxBe.s!ag!Neg!Cond!Simul + (pastverbsubjclictic.s!ag).p3,mkSuffix"a">
      }};



}
