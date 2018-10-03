--# -path=.:../../prelude

--1 Egekusii Resource Morphology

resource MorphoGus = CommonBantu ,
ResGus ** open Prelude, Predef
in {

  flags optimize=all ;
  oper 
  Many_prefix: Gender ->  Str = \g ->
   case <g> of {    
   <G1>  =>"aba";
    <G4> |<G9>|<G8> =>"ama";
    <G3> |<G6>  =>"cini";
    <G2>  =>"eme";
    <G5> | <G7> =>"ebi";
    <G10> => "ani"
      } ;

  Few_prefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"basi";
    <G4> |<G9>|<G8> =>"masi";
    <G3> |<G6>  =>"cisi";
    <G2>  =>"mesi";
    <G5> | <G7> =>"bisi";
    <G10> => "asi"
      } ;


  Detsomesgprefix : Gender ->  Str = \g ->"";
  {-} case <g> of {    
    <G3> => "li" ;
    <G4>   => "ki" ;
    <G9>  => "me";
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G5>|<G10> => "nyi" ; 
    <G1> |<G6>|<G2>|<G7>|<G8> |<G13> => "mwi" 

      } ; -}
    Some_prefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"beke";
    <G7> =>"bike";
    <G8>  =>"make";
    <G4> =>"make";
    <G3>  =>"nke";
    <G6> =>"nke";
    <G2>  =>"mebe";
    <G5> =>"bike";
    <G9> =>"make";
    <G10> => "ake"
      } ;

Detsomeplprefix : Gender ->  Str = \g ->"";
 {-}  case <g> of {    
    <G1>   => "we" ;
    <G2>   => "mi"  ;
    <G10> => "nyi" ;
    <G11> => "pe" ;
    <G12> => "kwi" ;
    <G4>    => "vi" ;
     <G5>|<G6> => "nye" ;    
    <G7> |<G13>  => "mwi" ;
     <G3>|<G8> |<G9>  => "me"

      } ; -}

 mkNum3 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++  second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ CardThirteenprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++  second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etato"  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etato"};
       hund  => table {NCard =>\\g =>"amagana atato " ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atato"}  
       }
    } ;
  mkNum4 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfouteenprefix g ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ second } ; 
       ten  => table {NCard =>\\g =>"emerongo  ene"; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  ene"};
       hund  => table {NCard =>\\g =>"amagana ane " ; 
                      NOrd => \\g => Ordprefix g ++ "amagana ane"}  
       }
    } ;
    mkNum5 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ;-- create table totake care of eci and oroci which is isano not itano 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ second } ; 
       ten  => table {NCard =>\\g =>"emerongo  etano"  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etano"};
       hund  => table {NCard =>\\g =>"amagana atano " ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"}  
       }
    } ;

    mkNum6 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++Cardsixprefix g + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ri" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardsixprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++ "ri" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano" ++ Cardsixprefix g + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etato"++ "ri" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardoneprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++ "ri" + second}  
       }
    } ;

    mkNum7 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++Cardtwoprefix g + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ka" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardtwoprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++  "ka" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano" ++ Cardtwoprefix g + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etano"++  "ka" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardtwoprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++"ka" + second}  
       }
    } ;

     mkNum8 : Str  -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++ Cardprefix g   + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ga" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++  "ga" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano" ++ Cardprefix g   + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etano"++ "ga" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++ "ga" + second}  
       }
    } ;
   mkNum2 : Str ->  Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two,  second ->
    {s = table {
       unit => table {NCard =>\\g => Cardtwoprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardtwelveprefix g + two ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na"  ++ Cardtwelveprefix g + two } ; 
       ten  => table {NCard =>\\g =>"emerongo  ebere"  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo ebere" };
       hund  => table {NCard =>\\g =>"amagana  ebere" ; 
                      NOrd => \\g => Ordprefix g ++ "amagana ebere" }  
       }
    } ;

    mkNum1 : Str -> Str -> {s : DForm => CardOrd => Gender => Str} = 
    \two,  second -> 
    {s = table {
       unit => table {NCard =>\\g => Cardoneprefix g + two ; 
                      NOrd => \\g => Ordoneprefix g ++ second} ; 
       teen => table {NCard =>\\g =>"ikomi  nemo" ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi " ++ "nemo"} ; 
       ten  => table {NCard =>\\g =>"ikomi" ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi"};
       hund  => table {NCard =>\\g =>"rigana  erimo"; 
                      NOrd => \\g => Ordprefix g ++ "rigana erimo" }  
       }
    } ;

  regNum : Str -> {s : DForm => CardOrd => Gender => Str} = 
    \six -> {s = table {
       unit => table {NCard =>\\g => six ; 
                      NOrd => \\g => Ordprefix g ++ six} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ six} ; 
       ten  => table {NCard =>\\g =>"emerongo"  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo" ++ six};
       hund  => table {NCard =>\\g =>"amagana "  ++ six ; 
                      NOrd => \\g => Ordprefix g ++ "amagana" ++ six}  
       } } ;

 
  regCardOrd : Str -> {s : CardOrd => Gender => Str} = \ten ->
    {s = table {NCard => \\g =>  ten ; 
    NOrd =>\\g => Ordprefix g ++ ten } } ;  

    regCardone : Str -> Str -> {s : CardOrd => Gender => Str} = \ten,one ->
    {s = table {NCard => \\g =>  ten ++ Cardoneprefix g + one ; 
    NOrd =>\\g => Ordprefix g ++ ten ++ Cardoneprefix g + one  } } ;

  mkCard : CardOrd -> Str -> Gender => Str = \o,ten -> 
    (regCardOrd ten).s ! o ; 
 
    
  
 

regN : Str ->Gender -> Noun =  \w, g ->let 
       ndeto= PrefixPlNom g + Predef.drop 3 w;
           in case g of {
       G4 => {s = table {  Sg => table {Nom => w; Loc =>  ""}  ;
                Pl=>table{ Nom => "ama" + Predef.drop 2 w; Loc =>  "" }  
                }; g = g} ;
       G3=> {s = table {  Sg => table {Nom => w; Loc =>  ""}  ;
                Pl=>table{ Nom => "ci" + Predef.drop 1 w; Loc => "" } 
                 }; g = g} ;
      _ => {s = table {  Sg => table {Nom => w; Loc =>  ""}  ;
                Pl=>table{ Nom => ndeto; Loc =>  "" }  };
                 g = g}};

  iregN :Str-> Str ->Gender -> Noun= \man,men,g -> { 
    s = table{Sg => table{Nom => man ; Loc=> ""}; 
              Pl => table{Nom => men ; Loc=> ""}} ;
    g = g
    } ;

 regA:Str -> {s : AForm =>  Str} = \seo ->  {s = table {
     AAdj G1  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"u"  => "omu" + seo;
                "o"  => "omw" + seo;
                   _ => ConsonantAdjprefix  G1 Sg + seo };
     AAdj G1  Pl =>case Predef.take 1 seo of { 
                   _ => ConsonantAdjprefix  G1 Pl + seo };

  
    AAdj G2   Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"u"  => "omu" + seo;
                "o"  => "omw" + seo;
                   _ => ConsonantAdjprefix  G2 Sg + seo };
    AAdj  G2  Pl =>case Predef.take 1 seo of { 
              "o" => "emi" + seo;
             _ => ConsonantAdjprefix  G2 Pl + seo };
  
    AAdj G3  Sg=>case Predef.take 1 seo of { 
               "o" |"i"  => "eng" + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
   AAdj G3 Pl =>case Predef.take 1 seo of { 
               "o" |"i"  => "ching" + seo;
                _ => ConsonantAdjprefix  G3 Pl + seo };
    AAdj G4  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => "rigi" + seo;
                   _ => ConsonantAdjprefix  G4 Sg + seo };
      AAdj G4 Pl =>case Predef.take 1 seo of { 
                     _ => ConsonantAdjprefix  G4 Pl + seo };
    AAdj G5  Sg=>case Predef.take 1 seo of { 
               "i" => "eki" + seo;
                   _ => ConsonantAdjprefix  G5 Sg + seo };
      AAdj G5 Pl =>case Predef.take 1 seo of { 
                          "i"  => "ebi" + seo;
                   _ => ConsonantAdjprefix  G5 Pl + seo };

    AAdj G6  Sg=>case Predef.take 1 seo of { 
               "i"|"o"  => "oru"+ seo;
                   _ => ConsonantAdjprefix  G6 Sg + seo };
      AAdj G6 Pl =>case Predef.take 1 seo of { 
             "i"|"o"  => "ching'"+ seo;
                    _ => ConsonantAdjprefix  G6 Pl + seo };
   AAdj G7  Sg=>case Predef.take 1 seo of { 
                    _ => ConsonantAdjprefix  G7 Sg + seo };
      AAdj G7 Pl =>case Predef.take 1 seo of { 
                    _ => ConsonantAdjprefix  G7 Pl + seo };
    AAdj G8  Sg=>case Predef.take 1 seo of { 
               "i"|"o"  => "obu"+ seo;
                   _ => ConsonantAdjprefix  G8 Sg + seo };
      AAdj G8 Pl =>case Predef.take 1 seo of { 
                    _ => ConsonantAdjprefix  G8 Pl + seo };
      AAdj G9  Sg=>case Predef.take 1 seo of { 
               "i"|"o"  => "oku" + seo;
                   _ => ConsonantAdjprefix  G9 Sg + seo };
      AAdj G9 Pl =>case Predef.take 1 seo of { 
                                  _ => ConsonantAdjprefix  G9 Pl + seo };
  
     AAdj G10  Sg=>case Predef.take 1 seo of { 
                                  _ => ConsonantAdjprefix  G10 Sg + seo };
      AAdj G10 Pl =>[] }};

    
lregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + seo ++ seoo; 
             AAdj g Pl=> ProunPlprefix g + seo ++ seoo 
            } } ;
                      
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
            AAdj g Sg=> seo;
            AAdj g Pl => seoo} };

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


Cardtwelveprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"ba";
    <G2>  =>"ne";
     <_> => "i"
         } ;

CardThirteenprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> |<G5>=>"bat";
    <G8> |<G4> |<G9> =>"at";
    <G3> |<G6> =>"is";
     <G2> =>"nit";
     <G10> =>"at"
            } ;

Cardsixprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> =>"o";
    <G7> |<G5>=>"bi";
     <G2>|<G4> |<G3>|<G6> |<G8> |<G9> |<G10>  =>"e"
               } ;

  Cardfouteenprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> |<G5>=>"bane";
    <G8> |<G4> |<G9> =>"ane";
    <G3> |<G6> =>"inye";
     <G2> =>"ene";
     <G10> =>"ene"
            } ;
  Cardfifteenprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> =>"ba";
    <G5> |<G7>=>"bi";
    <G8> |<G9> |<G4> =>"a";
    <G3> |<G6> =>"es";
     <G2> =>"e";
     <G10> =>"a"
            } ;
       Ordoneprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"omo";
    <G4> => "rita";
    <G5>  => "ege";
    <G3> => "en";
    <G6>=> "oro";
    <G7>=> "aka";
    <G8>=> "abo";
    <G9>=> "oko";
    < G10> => "aa"
    } ;
        }
