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
     <Pres,_, Pos, AgP1 Pl > => "twi";
    -- <Pres, Pos, Ag G1 Sg P3> => "wi";
     <Pres,_, Pos, AgP3 _ _> => "ni";
     <Pres,_, Neg, AgP1  Sg > => "ndi";
     <Pres,_, Neg, AgP3 G1 Sg> | <Pres,_, Neg, AgP2  Sg>=> "ndwi";
     <Pres,_, Neg, AgP3 _ _> => "ti";
     <Past,_,Pos,AgP3 _ _> =>(auxillary.s!agr).p5  ;
     <Fut, Anter,Pos , AgP3 _ _> => (auxillary.s!agr).p1  ;
     <Fut, Anter,Neg, AgP3 _ _> => (auxillary.s!agr).p1 ;
     <Cond, Simul,Pos , AgP3 _ _> => (auxillary.s!agr).p1 ;
     <Cond, Anter,Pos , AgP3 _ _> => (auxillary.s!agr).p3  ;
     <Cond, Anter,Neg , AgP3 _ _> => (auxillary.s!agr).p4  ;
      <Cond, Simul,Neg, AgP3 _ _> =>  (auxillary.s!agr).p2;
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
            AgP1 Sg  =><"ni","ndi","na","ndy">;
            AgP2 Sg  => <"u","ndu","wa","ndw">;
            AgP3 G1 Sg  => <"u","ndu","wa","ndw">;
            AgP1 Pl  =><"tu","tu","twa","tuy">;
            AgP2 Pl  => <"mu","mu","mwa","muy">;
            AgP3 G1 Pl  => <"ma","ma","ma","may">;
            AgP3 G2 Sg=><"u","ndu","wa","ndw">;
            AgP3 G2 Pl =><"-","ndi","ya","ndy">;
            AgP3 G3 Sg =><"yi","yi","ya","yi">;
            AgP3 G3 Pl =><"ma","ma","ma","may">;
            AgP3 G4 Sg => <"ki","ki","kya","kiy">;
            AgP3 G4 Pl => <"-","i","sya","iy">;
            AgP3 G5 Sg => <"ka","ka","kaa","kay">;
            AgP3 G5 Pl => <"tu","tu","twa","tuy">;
            AgP3 G6 Sg => <"va","va","va","vay">;
            AgP3 G6 Pl =><"ku","ku","kwa","kuy">;
            AgP3 G7 Sg => <"-","nde","ya","ndy">;
            AgP3 G7 Pl =><"-","i","sya","iy">;
            AgP3 G8 Sg =><"u","nde","wa","ndw">;
            AgP3 G8 Pl =><"ma","ma","ma","may">;
            AgP3 G9 Sg =><"u","nde","wa","ndw">;
            AgP3 G9 Pl => <"-","i","sya","iy">;
            AgP3 G10 Sg => <"ku","ku","kwa","kuy">;
            AgP3 G10 Pl=><"ma","ma","ma","may">
           -- Ag _  _  _  =><"","","",""> 
            }};
       presverbsubjclitic : VerbSubjclitic = { s=\\a => case a of {
              AgP1 Sg =><"ni","ndi","na","ndi">;
            AgP2 Sg => <"nu","ndu","wa","ndu">;
            AgP3 G1 Sg => <"nu","nda","wa","nda">;
            AgP1 Pl  =><"tu","tui","twa","tui">;
            AgP2 Pl => <"mu","mui","mwa","mui">;
            AgP3 G1 Pl => <"ma","tia","ma","mai">;
            AgP3 G2 Sg =><"u","ndu","wa","ndu">;
            AgP3 G2 Pl  =><"i","ndi","ya","ndi">;
            AgP3 G3 Sg  =><"yi","yii","ya","yi">;
            AgP3 G3 Pl  =><"ma","mai","ma","mai">;
            AgP3 G4 Sg  => <"ki","kii","kya","ki">;
            AgP3 G4 Pl  => <"i","ii","sya","ii">;
            AgP3 G5 Sg  => <"ka","kai","ka","ka">;
            AgP3 G5 Pl  => <"tu","tui","twa","tui">;
            AgP3 G6 Sg  => <"va","vai","va","vai">;
            AgP3 G6 Pl  =><"ku","kui","kwa","kui">;
            AgP3 G7 Sg  => <"i","ndi","ya","ndi">;
            AgP3 G7 Pl  =><"i","syi","sya","syi">;
            AgP3 G8 Sg  =><"u","ndu","wa","ndu">;
            AgP3 G8 Pl  =><"ma","mai","ma","mai">;
            AgP3 G9 Sg  =><"u","ndu","wa","ndu">;
            AgP3 G9 Pl  => <"i","syi","ma","syi">;
            AgP3 G10 Sg => <"ku","kui","kwa","kui">;
            AgP3 G10 Pl=><"ma","mai","ma","mai">
           --Ag _  _  _  =><"","","",""> 
           }};
  
-- : Str -> Str = \c -> c ++ Predef.BIND ;
auxillary : VerbAux  = { s=\\a => case a of {
           AgP1 Sg =><"ngeethiwa","ndikeethiwa","ninesaa","ndeesaa" , "nai">;
            AgP2 Sg => <"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa","wai">;
            AgP3 G1 Sg => <"ukeethiwa","ndukeethiwa","niwesaa","ndesaa","wai">;
            AgP1 Pl =><"tukeethiwa","tutikeethiwa","nitwesaa","tuyesaa", "twai">;
            AgP2 Pl => <"mukeethiwa","muikeethiwa","nimwesaa","muyesaa","mwai">;
            AgP3 G1 Pl => <"makeethiwa","maikeethiwa","nimesaa","mayesaa","mai">;
            AgP3 G2 Sg =><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa","wai">;
            AgP3 G2 Pl  =><"ikeethiwa","ndikeethiwa","niyesaa","ndyesaa", "yai">;
            AgP3 G3 Sg  =><"ikeethiwa","yiikeethiwa","niyesaa","iyesaa", "yai">;
            AgP3 G3 Pl  =><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">;
            AgP3 G4 Sg  => <"kikeethiwa","kiikeethiwa","nikyesaa","kiyesaa", "kyai">;
            AgP3 G4 Pl  => <"ikeethiwa","iikeethiwa","nisyesaa","iyesaa", "syai">;
            AgP3 G5 Sg  => <"Kakeethiwa","Kaikeethiwa","nikesaa","kayesaa", "kai">;
            AgP3 G5 Pl  => <"Tukeethiwa","Tuikeethiwa","nitwesaa","tuyesaa", "twai">;
            AgP3 G6 Sg  => <"vakeethiwa","vaikeethiwa","nivesaa","vayesaa", "vai">;
            AgP3 G6 Pl  =><"kukeethiwa","kuikeethiwa","nikwesaa","kuyesaa", "kwai">;
            AgP3 G7 Sg  => <"ikeethiwa","ndikeethiwa","niyesaa","ndeesaa", "yai">;
            AgP3 G7 Pl  =><"syikeethiwa","syikeethiwa","nisyesaa","iyesaa", "syai">;
            AgP3 G8 Sg  =><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa", "wai">;
            AgP3 G8 Pl  =><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">;
            AgP3 G9 Sg  =><"ukeethiwa","ndukeethiwa","niwesaa","ndwesaa", "wai">;
            AgP3 G9 Pl  => <"syikeethiwa","syikeethiwa","nisyesaa","iyesaa", "syai">;
            AgP3 G10 Sg => <"kukeethiwa","kuikeethiwa","nikwesaa","kuyesaa", "kwai">;
            AgP3 G10 Pl =><"makeethiwa","maikeethiwa","nimesaa","mayesaa", "mai">
             --Ag _  _  _  =><"","","","",""> 
             }};



futverbsubjclitic  : VerbSubjclitic = { s=\\a => case a of {
            AgP1 Sg =><"ni","ndi","ni","nda">;
            AgP2 Sg => <"u","ndu","u","uta">;
            AgP3 G1 Sg => <"a","ndu","u","uta">;
            AgP1 Pl =><"tu","tuti","tu","tuta">;
            AgP2 Pl => <"mu","muti","mu","muta">;
            AgP3 G1 Pl => <"ma","mati","ma","mata">;
            AgP3 G2 Sg =><"u","ndu","u","uta">;
            AgP3 G2 Pl =><"i","ndi","i","ita">;
            AgP3 G3 Sg =><"i","ii","yi","yita">;
            AgP3 G3 Pl =><"ma","mai","ma","mata">;
            AgP3 G4 Sg => <"ki","kii","ki","kita">;
            AgP3 G4 Pl => <"i","ii","ii","syita">;
            AgP3 G5 Sg => <"ka","kai","ka","kata">;
            AgP3 G5 Pl => <"tu","tui","tu","tuta">;
            AgP3 G6 Sg => <"va","vai","va","vata">;
            AgP3 G6 Pl =><"ku","kui","ku","kuta">;
            AgP3 G7 Sg => <"i","ii","yi","yita">;
            AgP3 G7 Pl =><"i","syii","syi","syita">;
            AgP3 G8 Sg =><"u","ndu","u","uta">;
            AgP3 G8 Pl =><"ma","mai","ma","mata">;
            AgP3 G9 Sg =><"u","ndu","u","uta">;
            AgP3 G9 Pl => <"i","syii","syi","syita">;
            AgP3 G10 Sg => <"ku","kui","ku","kuta">;
            AgP3 G10 Pl=><"ma","mai","ma","mata">
          -- Ag _  _  _  =><nonExist,nonExist,nonExist,nonExist>
           }};

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
