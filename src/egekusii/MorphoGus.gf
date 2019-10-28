

--# -path=.:../../prelude

--1 Egekusii Resource Morphology

resource MorphoGus = CommonBantu ,
ResGus ** open Prelude, Predef
in {

  flags optimize=all ;
  oper 
  let_s: Str="";
  lets: Str=" ";
  subjectmarker:Agr-> Str =\ag ->"";
   form,forms: VForm= VGen;
  subject: Agr-> Str =\ag -> "";--unworked later(subjclitic.s!ag).p1;
   dQue: Str="";
  inQue: Str="ndwisi kana"; 
  Poltemp : Type ={ s: Polarity => Tense => Anteriority =>  Agr => Str *Str}; 
  polanttense : Poltemp ;
  polanttense : Poltemp  ={s=\\p,t,a,ag => case <t,a,p> of {
        <Past, Anter, Pos> => < "konye" ++ (subjclitic.s!ag).p3 ,[]> ; 
        <Past, Anter, Neg> => <"konye" ++ (subjclitic.s!ag).p4 ,[]>; 
        <Past, Simul,Pos> => <(subjclitic.s!ag).p1,[] >; ---some , removed ka
        <Past, Simul,Neg> => <(subjclitic.s!ag).p6,[]> ; -- for "ti" consder oper since some agreement dont take it
        <Pres, Simul,Pos> => <(subjclitic.s!ag).p1,[] >; ---done
        <Pres, Simul,Neg> => <(subjclitic.s!ag).p5 + "ri" ++ "go" ,[] >; 
        <Pres, Anter,Pos> => <(subjclitic.s!ag).p3,[]> ;
        <Pres, Anter,Neg> => <(subjclitic.s!ag).p4,[]> ;
        <Fut, Simul,Pos> => <(subjclitic.s!ag).p1 + "gocha",[]> ; ---done
        <Fut, Simul,Neg> => <(subjclitic.s!ag).p5,[]>; 
        <Fut, Anter,Pos> => < "onyore " ++ (subjclitic.s!ag).p3,[]>;
        <Fut, Anter,Neg> => <"tokonyora" ++(subjclitic.s!ag).p3,[] > ;
        <Cond, Simul,Pos> => <(subjclitic.s!ag).p1 ,[]> ;---done 
        <Cond, Anter,Neg> => <"tokonyora" ++ (subjclitic.s!ag).p3,[]>  ;
        <Cond, Anter,Pos> => < "oranyore   konye" ++(subjclitic.s!ag).p3,[]> ;
        <Cond, Simul,Neg> => <(subjclitic.s!ag).p2,[] >
      }};
 
 VerbSubjclitic : Type = {s : Agr => Str * Str* Str* Str* Str* Str };
  subjclitic : VerbSubjclitic = { s=\\a => case a of {
             Ag G1 Sg P1 =><"n","tinare "," na","  tinda","tindi","ti">;
            Ag G1 Sg P2 => <"o","nkware"," twa","  tora","to","to">;
            Ag G1 Sg P3 => <"a","tare "," kwa","  tora","ta","ta">;
            Ag G1 Pl P1 =><" nto","ntware "," mwa","ntora","nto","nto">; ---change nto into twa
            Ag G1 Pl P2 => <"mo","timware "," o","  tara","mo","mo">;
            Ag G1 Pl P3 => <"ba","mbare "," ba","   mbara","mba","mba">;
            Ag G2 Sg P3=><"o","tore "," o","  tora","to","to">;
            Ag G2 Pl P3 =><"e","tiyare "," e","  tera","te","te">;
            Ag G3 Sg P3 =><"e","tiyare "," e","  tera","te","te">;
            Ag G3 Pl P3 =><"chi","tichiare "," chia","  chira","chita","chi">;
            Ag G4 Sg P3 => <"ri","ndiare ","  ria","  ndira","ndi","ndi">;
            Ag G4 Pl P3 => <"a","tare "," a","  atara","ata","ata">;
            Ag G5 Sg P3 => <"ge","Ngeri "," kia","  gera","geta","ge">;
            Ag G5 Pl P3 => <"bi","Mbiri ","  bia","  bira","bita","bi">;
            Ag G6 Sg P3 => <"ro","ndware "," rwa","  ndora","rota","ro">;
            Ag G6 Pl P3 =><"chi","tichiare "," chia","  chira","chita","chi">;
            Ag G7 Sg P3 => <"ga","nkare"," ka","  gara","gata","ga">;
            Ag G7 Pl P3 =><"bi","Mbiare "," bia","   bira","bita","bi">;
            Ag G8 Sg P3 =><"bo","tibware "," bwa","   bora","bota","bo">;
            Ag G8 Pl P3 =><"a","tare "," a","  atara","ata","ata">;
            Ag G9 Sg P3 =><"go","nkware"," gwa","  gotara","gota","go">;
            Ag G9 Pl P3 => <"a","tare "," a","  atara","ata","ata">;
            Ag G10 Sg P3 => <"a","tare ","  a","  atara","ata","ata">;
            Ag G10 Pl P3=><"","","  a"," tara","","">;
            Ag G11 Sg P3 =><"a","tare "," a","  tara","ata","ata">;
            Ag G11 Pl P3 =><"a","tare ","  a"," tara","ata","ata">;
            Ag  _  _  _ =><"","","","","","">
            }};

  
  auxMopheme : Agr -> Str = \a -> case a of {
            Ag G1 Sg P1 =>"ninde";
            Ag G1 Sg P2 => "nore";
            Ag G1 Sg P3 => "nare";
            Ag G1 Pl P1 =>"tore";
            Ag G1 Pl P2 => "more";
            Ag G1 Pl P3 => "bare";
            Ag G2 Sg P3=>"nore";
            Ag G2 Pl P3 =>"nere";
            Ag G3 Sg P3 =>"nore";
            Ag G3 Pl P3 =>"chire";
            Ag G4 Sg P3 => "ndire";
            Ag G4 Pl P3 => "are";
            Ag G5 Sg P3 => "kere";
            Ag G5 Pl P3 => "bire";
            Ag G6 Sg P3 => "rore";
            Ag G6 Pl P3 =>"chire";
            Ag G7 Sg P3 => "kare";
            Ag G7 Pl P3 =>"bire";
            Ag G8 Sg P3 =>"bore";
            Ag G8 Pl P3 =>"are";
            Ag G9 Sg P3 =>"";
            Ag G9 Pl P3 => "";
            Ag G10 Sg P3 => "";
            Ag G10 Pl P3=>"nare";
            Ag G11 Sg P3 =>"";
            Ag G11 Pl P3 =>"nare";
            Ag  _  _  _ =>""
       } ;

            mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;

  Many_prefix: Cgender ->  Str = \g ->
   case <g> of {    
   <G1>  =>"aba";
    <G4> |<G9>|<G8> |<G11> =>"ama";
    <G3> |<G6>  =>"cini";
    <G2>  =>"eme";
    <G5> | <G7> =>"ebi";
    <G10> => "ani"
      } ;

Few_prefix : Cgender ->  Str = \g ->
   case <g> of { 
    <G1>  =>"ba";
    <_> => "bi"}; 
    Some_prefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"beke";
    <G7> =>"bike";
    <G8>  =>"make";
    <G4> =>"make";
    <G3>  => "chi" | "nke";
    <G6> =>"chi" |"nke";
    <G2>  =>"mebe";
    <G5> =>"bike";
    <G9> | <G11> =>"make";
    <G10> => "ake"
      } ;



 

     mkNum : Str  -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g =>case two of {
                                    "ato" =>  Cardprefix g +"s" +two ; 
                                       _ =>  Cardprefix g +two  }; 
                      NOrd => \\g => Ordprefix g ++ second} ; 
       teen => table {NCard =>\\g =>case two of {
                      "bere" => "ikomi na"  ++ Cardtwelveprefix g + two ;
                      "ato" =>   "ikomi na"  ++ CardThirteenprefix g + two ;
                      "tano"=> "ikomi na"  ++ Cardfifteenprefix g + two ; 
                      "ne"=>  "ikomi na"  ++ Cardfouteenprefix g + two  }; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ second } ; 
       ten  => table {NCard =>\\g =>case two of {
                                    "ato" => "emerongo et" +two ; 
                                    "bere" |"tano" => "emerongo e" +two ;
                                       _ => "emerongo a" +two  };  
                      NOrd => \\g =>case two of {
                                    "ato" => Ordprefix g ++"emerongo et" +two ; 
                                       _ => Ordprefix g ++"emerongo a" +two  }};
       hund  => table {NCard =>\\g =>case two of {
                                    "bere" => "amagana e" +two ; 
                                    "ato" => "amagana et" +two ; 
                                       _ => "amagana a" +two  }; 
                      NOrd => \\g => case two of {
                                       "bere" => Ordprefix g ++"amagana e" +two ; 
                                       "ato" => Ordprefix g ++"amagana et" +two ;
                                          _ => Ordprefix g ++"amagana a" +two }}} } ;
   

    mkNum6 : Str  -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++ "na" ++Cardsixprefix g + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ri" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardsixprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++ "ri" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano no'" + Cardsixprefix g + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etato"++ "ri" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardoneprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++ "ri" + second}  
       }
    } ;

    mkNum7 : Str  -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++ "na" ++Cardtwoprefix g + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ka" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardtwoprefix g + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++  "ka" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano ne" ++ Cardtwoprefix g + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etano"++  "ka" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardtwoprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++"ka" + second}  
       }
    } ;

     mkNum8 : Str  -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two, second ->
    {s = table {
       unit => table {NCard =>\\g => Cardprefix g + two ++ "na" ++ Cardprefix g   + second;
                      NOrd => \\g => Ordprefix g ++ "ga" + two ++ "ga" + second} ; 
       teen => table {NCard =>\\g =>"ikomi na"  ++ Cardfifteenprefix g + two ++ Cardprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi na" ++ "ga" + two ++  "ga" + second} ; 
       ten  => table {NCard =>\\g =>"emerongo  etano ne" ++ Cardprefix g   + second  ; 
                      NOrd => \\g => Ordprefix g ++ "emerongo  etano"++ "ga" + second};
       hund  => table {NCard =>\\g =>"amagana atano "++ Cardprefix g  + second ; 
                      NOrd => \\g => Ordprefix g ++ "amagana atano"++ "ga" + second}  
       }
    } ;
  

    mkNum1 : Str -> Str -> {s : DForm => CardOrd => Cgender => Str} = 
    \two,  second -> 
    {s = table {
       unit => table {NCard =>\\g => Cardoneprefix g + two ; 
                      NOrd => \\g => Ordoneprefix g + second} ; 
       teen => table {NCard =>\\g =>"ikomi  nemo" ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi " ++ "nemo"} ; 
       ten  => table {NCard =>\\g =>"ikomi" ; 
                      NOrd => \\g => Ordprefix g ++ "ikomi"};
       hund  => table {NCard =>\\g =>"rigana  erimo"; 
                      NOrd => \\g => Ordprefix g ++ "rigana erimo" }  
       }
    } ;

  mkNum9 : Str -> {s : DForm => CardOrd => Cgender => Str} = 
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

 
  regCardOrd : Str -> {s : CardOrd => Cgender => Str} = \ten ->
    {s = table {NCard => \\g =>  ten ; 
    NOrd =>\\g => Ordprefix g ++ ten } } ;  

    regCardone : Str -> Str -> {s : CardOrd => Cgender => Str} = \ten,one ->
    {s = table {NCard => \\g =>  ten ++ Cardoneprefix g + one ; 
    NOrd =>\\g => Ordprefix g ++ ten ++ Cardoneprefix g + one  } } ;

  mkCard : CardOrd -> Str -> Cgender => Str = \o,ten -> 
    (regCardOrd ten).s ! o ; 
 
    
 
 regN : Str ->Cgender -> Noun =  \w, g -> let wpl = case g of {
              G1 =>case w of { 
                         "omwo" + _  => "aba" + Predef.drop 3 w ; 
                         "omw" + _  => "ab" + Predef.drop 3 w ; 
                          _   =>  PrefixPlNom G1  + Predef.drop 3 w};
             G2 =>case w of { 
                         "omw" + _  => "emi" + Predef.drop 3 w ; 
                          _   =>  PrefixPlNom G2  + Predef.drop 3 w};
              G3 => "chi" + Predef.drop 1 w;                
              G4=> case w of { "ri" + _  => "ama" + Predef.drop 2 w ;  
                           _   =>  PrefixPlNom G4  + Predef.drop 1 w};
              G10 =>  []; 
              G11=>   w;
              _ => PrefixPlNom g  + Predef.drop 3 w};                   
          in mkNoun w wpl g ;

          iregN :Str-> Str ->Cgender -> Noun= \man,men,g ->mkNoun man men g;

  mkNoun :Str-> Str ->Cgender -> Noun= \man,men,g -> { 
    s = table{Sg => table{Nom => man ; Loc=>case g of { G3 => man ++ "ime" ; _=>""}}; 
              Pl => table{Nom => men ; Loc=> case g of { G3 => men ++ "ime" ; _=>""}}} ;
    g = g
    } ;


regA :Str->{s : AForm =>  Str}= \adj ->regAdj adj [];
  regAdj:Str -> Str-> {s : AForm =>  Str} = \seo,see ->  {s = table {
-- regA:Str -> {s : AForm =>  Str} = \seo ->  {s = table {
     AAdj G1  Sg=>case Predef.take 1 seo of { 
               "a"|"i"|"u"  => "omu" + seo;
                "o" |"e" => "omw" + seo;
                   _ => ConsonantAdjprefix  G1 Sg + seo };
     AAdj G1  Pl =>case Predef.take 1 seo of { 
                   _ => ConsonantAdjprefix  G1 Pl + seo };

  
    AAdj G2   Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"u"  => "omu" + seo;
                "o"  => "omw" + seo;
                "b" => "em" + seo;
                 _ => ConsonantAdjprefix  G2 Sg + seo };
    AAdj  G2  Pl =>case Predef.take 1 seo of { 
              "o" |"y" => "emi" + seo;
              "b" => "em" + seo;
             _ => ConsonantAdjprefix  G2 Pl + seo };
  
    AAdj G3  Sg=>case Predef.take 1 seo of { 
               "o" |"i"  => "eng" + seo;
               "y" => "engi" + seo;
               "b" => "em" + seo;
                "e" => "eny" + seo;
                   _ => ConsonantAdjprefix  G3 Sg + seo };
   AAdj G3 Pl =>case Predef.take 1 seo of { 
               "o" |"i"  => "ching" + seo;
               "b" => "chim" + seo;
               "y"  => "chingi" + seo;
                _ => ConsonantAdjprefix  G3 Pl + seo };
    AAdj G4  Sg=>case Predef.take 1 seo of { 
               "a"|"e"|"i"|"o"|"u"  => "rigi" + seo;
                   _ => ConsonantAdjprefix  G4 Sg + seo };
      AAdj G4 Pl =>case Predef.take 1 seo of { 
                     _ => ConsonantAdjprefix  G4 Pl + seo };
    AAdj G5  Sg=>case Predef.take 1 seo of { 
               "y"|"i" => "eki" + seo;
               "g" => "eke" + seo;
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
   AAdj G11  Sg=>case Predef.take 1 seo of { 
               "e"|"o"  => "am" + seo;
                   _ => ConsonantAdjprefix  G11 Sg + seo };
      AAdj G11 Pl =>case Predef.take 1 seo of { 
                        "e"|"o"  => "am" + seo;
                                  _ => ConsonantAdjprefix  G11 Pl + seo };
  
     AAdj G10  Sg=>case Predef.take 1 seo of { 
                                  _ => ConsonantAdjprefix  G10 Sg + seo };
      AAdj G10 Pl =>[];
    Advv => see
  }   }; 
      -- }};

    
sregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g + seo ++ seoo; 
             AAdj g Pl=> ProunPlprefix g + seo ++ seoo;
             Advv=> [] 
            } } ;
cregA : Str->  {s : AForm =>  Str} = \seo -> {  
       s = table {
             AAdj g Sg => ProunSgprefix g ++ "eragi ya"  ++ seo; 
             AAdj g Pl=> ProunPlprefix g ++ "eragi ya"  ++ seo;
             Advv=> []} } ;   
                              
iregA : Str-> Str -> {s : AForm =>  Str} = \seo,seoo -> {  
       s = table {
            AAdj g Sg=> seo;
            AAdj g Pl => seoo;
            Advv=> []} };
prefixvoice : Str -> Str = \root ->
    case  root of {
      "t"+ _|"k"+ _|"ch"+ _|"s"+ _ =>   "ga" + root ; ---voiceless consonants
           _   => "ka" + root} ;  --voiced consonants 

   
     voiced_less : Str -> Str = \root ->
    case  root of {
      "t"+ _|"k"+ _|"ch"+ _|"s"+ _ =>   "go" + root ; ---voiceless consonants
           _   => "ko" + root} ;  --voiced consonants 

regV :Str -> Verb =\vika -> 
            let  stem = init vika in
  mkVerb vika  (voiced_less vika)  (stem + "eti")(stem + "ire")(voiced_less vika) ;
  iregV : Str ->Str ->Str ->Str ->Str -> Verb =\gen,fut,neg,anti,inf  -> mkVerb gen fut neg anti inf ;

 mkVerb :(gen,fut,neg,anti,inf : Str) -> Verb= \gen,fut,neg,anti,inf ->
      { s =table{ 
             VFut =>fut ;
             VNeg   => neg;
             VGen => gen;
             Vanter => anti;
             VInf => inf ;
             VExtension type=> init gen + extension  type + last gen
             };
    s1 =\\ pol,tes,ant,ag => let
      v_prefix = (polanttense.s!pol!tes!ant!ag).p1 ;
      v2 = (polanttense.s!Pos!Past!Simul!ag).p1 + neg;
    v3 = (polanttense.s!Pos!Past!Simul!ag).p1 + gen;
     gene= Predef.drop 2 gen;
       in
      case < tes, ant,pol > of {
        <Fut, Simul, Pos> => v_prefix ++ fut  ;
        <Past, Simul, Neg> => v_prefix + neg; 
        <Past, Simul, Pos> => case Predef.take 2 gen of { 
               "ka"  => v_prefix + (prefixvoice  gene)  ;
                  _ => v_prefix + (prefixvoice gen)};
        <Past, Anter, Neg> |<Pres, Anter, Neg> =>v_prefix + gen  ;
        <Cond, Simul,_> | <Pres, Simul, _> |<Fut, Simul, Neg> => v_prefix + inf ;
        <_, _,_> => v_prefix  + anti  };
    s2=\\pol,tes,ant,ag =>  case <tes ,ant,pol> of {
        <_,_, Neg> =>(subjclitic.s!ag).p5 + inf ;
        <Past,Simul,Pos> =>(subjclitic.s!ag).p1 + "renge" ++inf ;
        <_,_,Pos> =>(subjclitic.s!ag).p1 +inf };
        progV= [];
        imp=\\po,imf => case <po,imf> of {
                    <Pos,_> =>  gen;
                    <Neg, _> => "" }}; 


auxBe : VerbPhrase= { s =\\ag ,pol,tense,anter =>
    case < tense ,pol> of {
     <Pres, Neg> => (subjclitic.s!ag).p1 ++ "ri";
     <Pres, Pos> => auxMopheme ag;
     <Fut, Pos> => (subjclitic.s!ag).p1 + "be";
     <Fut, Neg> => (subjclitic.s!ag).p5 + "koba";
     <Past, Neg> =>(subjclitic.s!ag).p5 + "renge";
     <Past, Pos> => (subjclitic.s!ag).p5+ "renge";
     <Cond, Pos> => [];
     <Cond, Neg> => [] };
      s1=\\_,_,_,_=> []; 
      progV= [];
      compl=\\_=> [] ;
      imp =\\po,imf => "";
      inf= ""};
      
auxProgBe : VerbPhrase= { 
   s = \\ ag , pol , tense , anter =>
    case < tense ,pol> of {
     <Pres, Neg> => [];
     <Pres, Pos> => [];
     <_, _> => []};-- auxBe.s !ag!pol!tense!anter};
      s1=\\_,_,_,_=> []; compl=\\_=> [] ; progV= [];
      imp =\\po,imf => "";
      inf= ""};
regVP : Verb -> VerbPhrase ;
  regVP2 : (Verb ** {c2 :  Preposition}) -> SlashVP = \verb ->regVP verb ** {c2 = verb.c2 } ; 
    regVP run  = { 
      s =\\ ag,pol,tes,ant =>run.s1!pol!tes!ant!ag; 
      s1=\\ag,pol,tes,ant => run.s2!pol!tes!ant!ag; 
      compl=\\_=> [];
      progV= [];
      imp=\\po,imf => run.imp!po!imf;
      inf= run.s!VInf };



Cardtwelveprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"ba";
    <G2>  =>"ne";
    < G11> => "";
     <_> => "i"
         } ;

CardThirteenprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> |<G5>=>"bat";
    <G8> |<G4> |<G9> =>"at";
    <G3> |<G6> =>"is";
     <G2> =>"nit";
     < G11> => "";
     <G10> =>"at"
            } ;

Cardsixprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> =>"o";
    < G11> => "";
    <G7> |<G5>=>"bi";
     <G2>|<G4> |<G3>|<G6> |<G8> |<G9> |<G10>  =>"e"
               } ;

  Cardfouteenprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> |<G5>=>"bane";
    <G8> |<G4> |<G9> =>"ane";
    <G3> |<G6> =>"inye";
     <G2> =>"ene";
     <G10> =>"ene";
     < G11> => ""
            } ;
  Cardfifteenprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> =>"ba";
    <G5> |<G7>=>"bi";
    <G8> |<G9> |<G4> =>"a";
    <G3> |<G6> =>"es";
     <G2> =>"e";
     < G11> => "";
     <G10> =>"a"
            } ;
       Ordoneprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"omo";
    <G4> => "rita";
    <G5>  => "ege";
    <G3> => "en";
    <G6>=> "oro";
    <G7>=> "aka";
    <G8>=> "abo";
    <G9>=> "oko";
    < G10> => "aa";
    < G11> => ""
    } ;
        }
