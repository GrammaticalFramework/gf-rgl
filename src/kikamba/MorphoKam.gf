--# -path=.:../../prelude

--Kikamba Resource Morphology
--
-- Benson Kituku 2017 -- 2018

resource MorphoKam = CommonBantu ,ResKam ** open Prelude, Predef
in {

  flags optimize=all ;

 oper 
  mkDet: (i, mine : Str) -> Number -> {s : DetForm =>  Str ; n : Number} = 
    \i,mine, m -> 
            { s = table {
       Sub  => i;
       Obj  g => Detprefix g + mine} ;
       n = m } ; 


 Detsomesgprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> |<G8> |<G9> => "u" ;
     <G3>   => "yi" ;
    <G4>   => "ki" ;
    <G5> => "ka" ;
    <G10> => "ku" ;
    <G6> => "va" ;
    <G7>   => "i" 
      } ;

Detsomeplprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G3> |<G8> |<G10> => "a" ;
     <G2>   => "i" ;
    <G4>   => "i" ;
    <G5> => "tu" ;
    <G6> => "ku" ;
    <G7> | <G9>  => "i" 

      } ;

    

 mkNum : Str -> Str -> Str -> Str -> {s : DForm => CardOrd => Gender => Str} = 
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
  
   mkNumn : Str -> Str -> Str -> Str -> {s : DForm => CardOrd => Gender => Str} = 
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

    mkNume : Str ->  Str -> Str -> {s : DForm => CardOrd => Gender => Str} = 
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

  regNum : Str -> {s : DForm => CardOrd => Gender => Str} = 
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

 
  regCardOrd : Str -> {s : CardOrd => Gender => Str} = \ten ->
    {s = table {NCard => \\g =>  ten ; 
		NOrd =>\\g => Ordprefix g ++ ten } } ;  

    regCardone : Str -> Str -> {s : CardOrd => Gender => Str} = \ten,one ->
    {s = table {NCard => \\g =>  ten ++ Cardoneprefix g + one ; 
    NOrd =>\\g => Ordprefix g ++ ten ++ Cardoneprefix g + one  } } ;

  mkCard : CardOrd -> Str -> Gender => Str = \o,ten -> 
    (regCardOrd ten).s ! o ; 

                               
          regN : Str ->Gender -> Noun =  \w, g -> let wpl = case g of {
              G1=>case w of {
                     "mwa" + _  =>  Predef.drop 2 w ; 
                     "mwi" + _  => "e"  + Predef.drop 3 w ;  
                      _  => PrefixPlNom G1  + Predef.drop 2 w }; 

              G2=>case w of {
                     "mw" + _  => "my"  + Predef.drop 2 w ; 
                      _  => PrefixPlNom G2  + Predef.drop 2 w };
              G3 => PrefixPlNom G3  + Predef.drop 1 w;      
              G4=> case w of {
                     "ky" + _  => "sy" + Predef.drop 2 w ;  
                      _   =>  PrefixPlNom G4  + Predef.drop 2 w };
              G5 => PrefixPlNom G5  + Predef.drop 2 w; 
              G6 => PrefixPlNom G6  + Predef.drop 2 w;
              G8 => PrefixPlNom G8  +  w;
              G9 => PrefixPlNom G9  + w ;
              G10 => PrefixPlNom G10  + Predef.drop 2 w;
              G7 =>  w };                   
          in iregN w wpl g ;
 




  iregN :Str-> Str ->Gender -> Noun= \man,men,g -> { -- for irregular noun
    s = table{Sg => table{Nom => man ; 
                          Loc=> man + "ni"  | men + "ni" }; 
              Pl => table{Nom => men ; Loc=> ""}} ;
    g = g
    } ;
  regA:Str -> {s : AForm =>  Str} = \seo ->  {s = table {
     AAdj G1  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"  => "mw" + seo;
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
               "i"|"u"  => "y" + seo;
                  "a"  => "yi" + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
      AAdj G3 Pl =>case Predef.take 1 seo of { 
               "u"  => "mo"+ Predef.drop 1 seo;
               _ => ConsonantAdjprefix  G3 Pl + seo };

  
      AAdj G4 Sg=>case Predef.take 1 seo of { 
               "i"  => "k" + seo;
                  "u" |"a"  => "ky" + seo;
                   _ => ConsonantAdjprefix  G4 Sg + seo };
      AAdj G4 Pl =>case Predef.take 1 seo of { 
               "u"  => "mb"+  seo;
                "i"  => "sy" + seo;
               "a"  => "nd" + seo;
               _ => ConsonantAdjprefix  G4 Pl + seo };
     AAdj G5 Sg=>case Predef.take 1 seo of { 
                 "u"  => "ko" + Predef.drop 1  seo;
                   _ => ConsonantAdjprefix  G5 Sg + seo };
     AAdj G5  Pl =>case Predef.take 1 seo of { 
               "u"  => "t"+ seo;
               "a" | "i"  => "tw" + seo;
                _ => ConsonantAdjprefix  G5 Pl + seo };

      AAdj G6 Sg=>case Predef.take 1 seo of { 
               "u"  => "vo" + Predef.drop 1 seo;
                   _ => ConsonantAdjprefix  G6 Sg + seo };
      AAdj G6 Pl =>case Predef.take 1 seo of { 
               "u"  => "k" + seo;
               "i"|"a"  => "kw" +  seo;
               _ => ConsonantAdjprefix  G6 Pl + seo };

     AAdj G7 n =>case Predef.take 1 seo of { 
               "s" |"i"  => "nz" +  seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nd" +  Predef.drop 1 seo;
                 _ => ConsonantAdjprefix  G7 n + seo };

      AAdj G9 Pl  =>case Predef.take 1 seo of { 
               "s" |"i"  => "nz" +  seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nd" +  Predef.drop 1 seo;
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
               "s" |"i"  => "nz" +  seo;
                 "v" | "u"  => "mb" + seo;
                  "k"  => "ng" + seo;
                  "t" | "a"  => "nd" + seo;
                 _ => ConsonantAdjprefix  G7 n + seo };
                 in init af + "ang" + last af;

AComp G9 Pl  =>let af : Str = case Predef.take 1 seo of { 
               "s" |"i"  => "nz" +  seo;
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
                   in init af + "ang" + last af
   
  }   };             


                      
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
             AAdj g Sg =>  seo; 
             AAdj g Pl=> seoo ;
            AComp g Sg =>  init  seo + "ang" + last seo;
            AComp g Pl => init  seoo + "ang" + last seoo}
                           
       }  ;

--regular expression for adjective colours
 cregA : Str->  {s : AForm =>  Str} = \seo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + "a" ++"langi" ++"wa"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a" ++"langi" ++"wa"  ++ seo;
            AComp g n =>  ""} } ;

regV : Str -> Verb = 
     \vika -> {
     s = table{  True => table{
       VInf        => vika;
       VPres g n p => Verbprefix g n p + vika; 
       VPast g n p => Verbprefix g n p + init vika + "ie" ;
       VFut  g n p => Verbfutureprefix g n p + vika 
            } ;
        False =>table {
        VInf       =>  "ndi" + vika;
       VPres g n p => neg (Ag g n p) False Pres + "na" + vika ; 
       VPast g n p => neg (Ag g n p) False Past + "ne" + vika ;
       VFut  g n p => neg (Ag g n p) False Fut + "ka" + vika    
       } 
     };     
     };

  neg : Agr -> Bool ->Tense -> Str = \a,b,t -> let 
        g = getGender a;
        n=getNumber a;
        p=getPerson a
      in case b of {True => [] ; False => negprefix g n t p} ;

  negprefix : Gender -> Number -> Tense -> Person -> Str =\g,n,t,p-> case <g,n,t,p> of {
            <G1,Sg,_,P1> => "ndi";
            <G1,Sg,_,P2> => "ndu";
            <G1,Sg,_,P3> => "ndu";
            <G1,Pl,_,P1> => "twi";
            <G1,Pl,_,P2> => "mwi";
            <G1,Pl,_,P3> => "mai";
            <G2,Sg,_,_> => "ndu";
            <G2,Pl,_,_> => "i";
            <G3,Sg,_,_> => "i";
            <G3,Pl,_,_> => "mai";
            <G4,Sg,_,_> => "ki";
            <G4,Pl,_,_> => "i";
            <G5,Sg,_,_> => "kai";
            <G5,Pl,_,_> => "tui";
            <G6,Sg,_,_> => "vai";
            <G6,Pl,_,_> => "kui";
            <G7,Sg,_,_> => "i";
            <_,_,_,_> => "syi"
           
};




   

 Verbprefix : Gender -> Number -> Person -> Str = \g, n, p ->
   case <g,n,p> of {    
    <G1,Sg,P1>   => "na" ;
    <G1,Sg,P2>   => "wa" ;
    <G1,Sg,P3>   => "wa" ;
    <G1,Pl,P1>   => "twa" ;
    <G1,Pl,P2>   => "mwa" ;
    <G1,Pl,P3>   => "ma" ;
    <G2,Sg,_>   => "wa" ;
    <G2,Pl, _>   => "ya"  ;
    <G4,Sg,_>   => "kya" ;
    <G4,Pl,_>   => "sya" ;
    <G3,Sg,_>  => "ya" ;
    <G3,Pl,_>  => "ma" ;
     <G5,Sg,_>  => "ka" ;
    <G5,Pl,_>  => "twa" ;
     <G6,Sg,_>  => "va" ;
    <G6,Pl,_>  => "kwa" ;
    <_,Sg,_>  => "ya" ;
    <_,Pl,_>  => "sya" 
          } ;
 
 Verbfutureprefix : Gender -> Number -> Person -> Str = \g, n, p ->
   case <g,n,p> of {    
    <G1,Sg,P1>   => "nga" ;
    <G1,Sg,P2>   => "uka" ;
    <G1,Sg,P3>   => "uka" ;
    <G1,Pl,P1>   => "tuka" ;
    <G1,Pl,P2>   => "muka" ;
    <G1,Pl,P3>   => "maka" ;
    <G2,Sg,_>   => "uka" ;
    <G2,Pl, _>   => "ika"  ;
    <G4,Sg,_>   => "kika" ;
    <G4,Pl,_>   => "ika" ;
    <G3,Sg,_>  => "ika" ;
    <G3,Pl,_>  => "maka" ;
     <G5,Sg,_>  => "kaka" ;
    <G5,Pl,_>  => "tuka" ;
     <G6,Sg,_>  => "vaka" ;
    <G6,Pl,_>  => "kuka" ;
    <_,Sg,_>  => "ika" ;
    <_,Pl,_>  => "ika" 
          } ;
}



