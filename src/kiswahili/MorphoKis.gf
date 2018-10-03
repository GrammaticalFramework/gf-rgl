--# -path=.:../../prelude

--1 Kiswahili morphology Resource Morphology
--
-- Benson Kituku 2017-2018


resource MorphoKis = CommonBantu ,ResKis 
** open Prelude, Predef 
in {

  flags optimize=all ;
  oper 
 
  Many_prefix : Gender ->  Str = \g ->
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


  Few_prefix : Gender ->  Str = \g ->
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

  Detsomesgprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G3> => "li" ;
    <G4>   => "ki" ;
    <G9>  => "me";
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G5>|<G10> => "nyi" ; 
    <G1> |<G6>|<G2>|<G7>|<G8> |<G13> => "mwi" 

      } ;

Detsomeplprefix : Gender ->  Str = \g ->
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

  

 
      mkNum : Str -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two,  second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ two} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ Cardprefix g + two} ; 
       ten  => table {NCard =>\\g =>second ++"na" ++ Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second ++"na" ++ Cardprefix g + two};
       hund  => table {NCard =>\\g =>"mia "  ++ two ;
                        NOrd => \\g => Ordprefix g ++ "mia "  ++ two }
       }
    } ;
  
   mkNumn : Str -> Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
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
       }
    } ;

    mkNume : Str ->  Str -> {s : DForm => CardOrd => Gender => Str} = 
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

  regNum : Str ->Str -> {s : DForm => CardOrd => Gender => Str} = 
    \six,sixth -> {s = table {
       unit => table {NCard =>\\g => six ; 
                      NOrd => \\g => Ordprefix g ++ six} ; 
       teen => table {NCard =>\\g =>"kumi na"  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "kumi na" ++ six} ; 
       ten  => table {NCard =>\\g =>sixth ++"na" ++ six ; 
                      NOrd => \\g => Ordprefix g ++ sixth ++"na" ++ six };
       hund  => table {NCard =>\\g =>"mia "  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "mia" ++ six}  
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
                   
          in iregN w wpl g ;
 




  iregN :Str-> Str ->Gender -> Noun= \man,men,g -> { -- for irregular noun
    s = table{Sg => table{Nom => man ; 
                          Loc=> man + "ni"  | men + "ni" }; 
              Pl => table{Nom => men ; Loc=> ""}} ;
    g = g
    } ;

 regA:Str -> {s : AForm =>  Str} = \seo ->  {s = table {
     AAdj G1 Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => VowelAdjprefix G1 Sg + seo;
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
    AAdj _  Pl =>[] }};

       


        
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
            AAdj g Sg=> seo;
            AAdj g Pl => seoo} };
            

   cregA : Str->  {s : AForm =>  Str} = \seo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + "a" ++"rangi" ++"ya"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a" ++"rangi" ++"ya"  ++ seo} } ;   

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
    <G7,Sg,_>  => "ya" ;
    <_,_,_>  => "sya" 
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
    <G7,Sg,_>  => "ika" ;
    <_,_,_>  => "ika" 
          } ;
}

