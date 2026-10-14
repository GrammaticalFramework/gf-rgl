--# -path=.:../../prelude

--1 Kiswahili morphology Resource Morphology
--
-- Benson Kituku 2017-2018


resource MorphoSwa = CommonBantu ,ResSwa 
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

regA:Str -> {s : AForm =>  Str} = \seo -> case seo of {
  -- Machine-translated lexica frequently provide a relational adjective as
  -- "a/wa/ya ..." or an -enye/relative form instead of an adjective root.
  -- Inflect the grammatical prefix, not the first letters of the phrase.
  "a " + rest      => phraseA genitivePrefix rest ;
  "wa " + rest     => phraseA genitivePrefix rest ;
  "ya " + rest     => phraseA genitivePrefix rest ;
  "cha " + rest    => phraseA genitivePrefix rest ;
  "mwenye " + rest => phraseA enyePrefix rest ;
  "Mwenye " + rest => phraseA enyePrefix rest ;
  "wenye " + rest  => phraseA enyePrefix rest ;
  "yenye " + rest  => phraseA enyePrefix rest ;
  "Yenye " + rest  => phraseA enyePrefix rest ;
  "chenye " + rest => phraseA enyePrefix rest ;
  "Chenye " + rest => phraseA enyePrefix rest ;
  "enye " + rest   => phraseA enyePrefix rest ;
  "isiyo" + rest   => phraseA relNegPrefix rest ;
  "Isiyo" + rest   => phraseA relNegPrefix rest ;
  "asiye" + rest   => phraseA relNegPrefix rest ;
  "Asiye" + rest   => phraseA relNegPrefix rest ;
  "inayo" + rest   => phraseA relPosPrefix rest ;
  "Inayo" + rest   => phraseA relPosPrefix rest ;
  _ => {s = table {
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
    AAdj _  Pl =>[] }}
  } ;

  phraseA : (Gender -> Number -> Str) -> Str -> {s : AForm => Str} =
    \prefix,rest -> {s = \\f => case f of {
      AAdj g n => prefix g n ++ rest
      }} ;

  joinedPhraseA : (Gender -> Number -> Str) -> Str -> {s : AForm => Str} =
    \prefix,rest -> {s = \\f => case f of {
      AAdj g n => prefix g n + rest
      }} ;

  genitivePrefix : Gender -> Number -> Str = \g,n -> possess_Prepof ! n ! g ;

  enyePrefix : Gender -> Number -> Str = \g,n -> case <g,n> of {
    <G1,Sg> => "mwenye" ; <G1,Pl> => "wenye" ;
    <G2,Sg> => "mwenye" ; <G2,Pl> => "yenye" ;
    <G3,Sg> => "lenye"  ; <G3,Pl> => "yenye" ;
    <G4,Sg> => "chenye" ; <G4,Pl> => "vyenye" ;
    <G5,Sg> => "yenye"  ; <G5,Pl> => "zenye" ;
    <G6,Sg> => "wenye"  ; <G6,Pl> => "zenye" ;
    <G7,_>  => "wenye" ;
    <G8,Sg> => "wenye"  ; <G8,Pl> => "yenye" ;
    <G9,_>  => "yenye" ; <G10,_> => "yenye" ;
    <_,_> => "yenye"
    } ;

  relPosPrefix : Gender -> Number -> Str = \g,n -> case <g,n> of {
    <G1,Sg> => "anaye"  ; <G1,Pl> => "wanao" ;
    <G2,Sg> => "unao"   ; <G2,Pl> => "inayo" ;
    <G3,Sg> => "linalo" ; <G3,Pl> => "yanayo" ;
    <G4,Sg> => "kinacho"; <G4,Pl> => "vinavyo" ;
    <G5,Sg> => "inayo"  ; <G5,Pl> => "zinazo" ;
    <G6,Sg> => "unao"   ; <G6,Pl> => "zinazo" ;
    <G7,_>  => "unao" ;
    <G8,Sg> => "unao"   ; <G8,Pl> => "yanayo" ;
    <G9,_>  => "yanayo" ; <G10,_> => "inayo" ;
    <_,_> => "inayo"
    } ;

  relNegPrefix : Gender -> Number -> Str = \g,n -> case <g,n> of {
    <G1,Sg> => "asiye"  ; <G1,Pl> => "wasio" ;
    <G2,Sg> => "usio"   ; <G2,Pl> => "isiyo" ;
    <G3,Sg> => "lisilo" ; <G3,Pl> => "yasiyo" ;
    <G4,Sg> => "kisicho"; <G4,Pl> => "visivyo" ;
    <G5,Sg> => "isiyo"  ; <G5,Pl> => "zisizo" ;
    <G6,Sg> => "usio"   ; <G6,Pl> => "zisizo" ;
    <G7,_>  => "usio" ;
    <G8,Sg> => "usio"   ; <G8,Pl> => "yasiyo" ;
    <G9,_>  => "yasiyo" ; <G10,_> => "isiyo" ;
    <_,_> => "isiyo"
    } ;

       


        
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
            AAdj g Sg=> seo;
            AAdj g Pl => seoo} };
            

   cregA : Str->  {s : AForm =>  Str} = \seo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + "a" ++"rangi" ++"ya"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g + "a" ++"rangi" ++"ya"  ++ seo} } ;   

regV : Str -> Verb =
     \input -> let vika = verbStem input in {
     s = verbForms vika ;
     passive = verbForms (passiveStem vika)
     };

  verbForms : Str -> Polarity => VForm => Str = \vika ->
     table{  Pos => table{
       VInf        => "ku" + vika;
       VImp Sg     => vika;
       VImp Pl     => pluralImperative vika;
       VPres g n p => Verbprefix g n p + "na" + vika;
       VPast g n p => Verbprefix g n p + "li" + vika;
       VFut  g n p => Verbprefix g n p + "ta" + vika
            } ;
             Neg =>table {
       VInf        => "kutoku" + vika;
       VImp Sg     => "usi" + vika;
       VImp Pl     => "msi" + vika;
       VPres g n p => negprefix g n Pres p + presentNegStem vika;
       VPast g n p => negprefix g n Past p + "ku" + vika;
       VFut  g n p => negprefix g n Fut p + "ta" + vika
       } 
     } ;

  passiveStem : Str -> Str = \v -> case v of {
    _ + "oa" => init (init v) + "olewa" ;
    _ + "ua" => init (init v) + "uliwa" ;
    _ + "ia" => init v + "wa" ;
    _ + "ea" => init v + "wa" ;
    _ + "a" => init v + "wa" ;
    _ + "u" => init v + "iwa" ;
    _ => v + "wa"
    } ;

  -- Lexical sources sometimes store an infinitive rather than a stem.  Strip
  -- ku- before vowel-initial polysyllabic stems, while retaining the genuine
  -- monosyllabic stems used by kuwa/kula/kufa/kuja and the verb kua.
  verbStem : Str -> Str = \v -> case v of {
    "kua" => v ;
    "ku" + "a" + rest => "a" + rest ;
    "ku" + "e" + rest => "e" + rest ;
    "ku" + "i" + rest => "i" + rest ;
    "ku" + "o" + rest => "o" + rest ;
    "ku" + "u" + rest => "u" + rest ;
    _ => v
    } ;

  pluralImperative : Str -> Str = \v -> case v of {
    _ + "a" => init v + "eni" ;
    _ => v + "ni"
    } ;

  presentNegStem : Str -> Str = \v -> case v of {
    _ + "a" => init v + "i" ;
    _ => v
    } ;

  negprefix : Gender -> Number -> Tense -> Person -> Str =\g,n,t,p-> case <g,n,t,p> of {
            <G1,Sg,_,P1> => "si";
            <G1,Sg,_,P2> => "hu";
            <G1,Sg,_,P3> => "ha";
            <G1,Pl,_,P1> => "hatu";
            <G1,Pl,_,P2> => "ham";
            <G1,Pl,_,P3> => "hawa";
            <G2,Sg,_,_> => "hau";
            <G2,Pl,_,_> => "hai";
            <G3,Sg,_,_> => "hali";
            <G3,Pl,_,_> => "haya";
            <G4,Sg,_,_> => "haki";
            <G4,Pl,_,_> => "havi";
            <G5,Sg,_,_> => "hai";
            <G5,Pl,_,_> => "hazi";
            <G6,Sg,_,_> => "hau";
            <G6,Pl,_,_> => "hazi";
            <G9,_,_,_> => "haya";
            <G11,_,_,_> => "hapa";
            <G12,_,_,_> => "haku";
            <G13,_,_,_> => "hamu";
            <_,_,_,_> => "ha"
           
};


 Verbprefix : Gender -> Number -> Person -> Str = \g, n, p ->
   case <g,n,p> of {    
    <G1,Sg,P1> => "ni" ; <G1,Sg,P2> => "u" ; <G1,Sg,P3> => "a" ;
    <G1,Pl,P1> => "tu" ; <G1,Pl,P2> => "m" ; <G1,Pl,P3> => "wa" ;
    <G2,Sg,_> => "u" ; <G2,Pl,_> => "i" ;
    <G3,Sg,_> => "li" ; <G3,Pl,_> => "ya" ;
    <G4,Sg,_> => "ki" ; <G4,Pl,_> => "vi" ;
    <G5,Sg,_> => "i" ; <G5,Pl,_> => "zi" ;
    <G6,Sg,_> => "u" ; <G6,Pl,_> => "zi" ;
    <G7,_,_> => "u" ; <G8,Sg,_> => "u" ; <G8,Pl,_> => "ya" ;
    <G9,_,_> => "ya" ; <G10,_,_> => "i" ;
    <G11,_,_> => "pa" ; <G12,_,_> => "ku" ; <G13,_,_> => "mu"
          } ;
 
 Verbfutureprefix : Gender -> Number -> Person -> Str = \g, n, p ->
   case <g,n,p> of {    
    <g,n,p> => Verbprefix g n p + "ta"
          } ;
}

