instance DiffKam of DiffBantu = open CommonBantu,Prelude in {

param 
  CgenderKam = G1 | G2 | G3 | G4 | G5 | G6 | G7| G8 | G9 |G10 ;
  VExte =  EPassive | EApplicative  |EReciprocal | ECausative |EDistributive ;
oper 
  Cgender = CgenderKam ;
  firstGender = G1 ; secondGender = G2 ;
  fixclass: Case -> Cgender -> Cgender=\ca,ge ->
  case <ca > of {
  <Nom > => ge;
    _ =>  G6};

  conjGender : Cgender -> Cgender -> Cgender = \m,n -> 
    case m of { G1 => n ; _ => G2 } ;

extension : VExte -> Str=\type ->case type of{
           EPassive => "w" ;
           EApplicative => "i" ; -- 
           EReciprocal  => "an" ;-- 
           EDistributive => "ang" ;
           ECausative => "ithy"  --
        };

  ProunSgprefix : Cgender ->  Str = \g ->  
    case g of {    
      G1 | G2|G8 |G9 => "w" ;
      G3 | G7 => "y" ;
      G4      => "ky" ;
      G5      => "k";
      G10      => "kw";
      G6      => "v"
    } ;

  ProunPlprefix : Cgender ->  Str = \g ->
    case g of {    
      G1 | G3 |G8 |G10 => "m" ;
      G2      => "y" ;
      G4 | G7 |G9 => "sy" ;
      G5      => "tw" ;
      G6      => "kw"
    } ;
Allpredetprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G3> |<G8> |<G10> => "" ;
    <G2>   => "y" ;
    <G4> | <G7> | <G9>  => "sy" ;
    <G5> => "two" ;
    <G6> => "kwo"
         } ;
 Mostpredetprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>   => "ala a" ;
    <G2>   => "ila m" ;
    <G3> | <G10>  => "ala  ma" ;
    <G4> | <G8>   => "ila mb" ;
    <G5> => "tula twi" ;
    <G6> => "kula kwi" ;
    <G7> | <G9>  => "ila mb" 
   
      } ;
 relPron : Cgender -> Number ->   Str = \g,n ->
   case <g,n> of {
     <G3,Sg> => "yila" ;
     <G4,Sg>  => "kila" ;
     <G5,Sg> => "kala" ;
    <G5,Pl> => "tula" ;
    <G6,Sg> => "vala" ;
    <G6,Pl>|<G10,Sg> => "kula" ;
    <G1,Sg> |<G2,Sg> |<G8,Sg> |<G9,Sg> => "ula" ;
    <G1,Pl> |<G3,Pl> |<G8,Pl> |<G10,Pl> => "ala" ;
    <G2,Pl> |<G9,Pl> |<G4,Pl> | <G7,Sg> | <G7,Pl> => "ila" 
           } ;
ConsonantAdjprefix: Cgender -> Number ->   Str = \g,n ->
   case <g,n> of {
    <G1,Sg> => "mu" ;
    <G1,Pl> => "a" ;
    <G2,Sg> => "mu" ;
    <G2,Pl> => "mi" ;
    <G3,Sg> => "i" ;
    <G3,Pl> => "ma" ;
    <G4,Sg> => "ki" ;
    <G4,Pl> => "" ;
    <G5,Sg> => "ka" ;
    <G5,Pl> => "tu" ;
    <G6,Sg> => "va" ;
    <G6,Pl> => "ku" ;
    <G7,n> => "" ;
    <G8,Sg> => "mu" ;
    <G8,Pl> => "ma" ;
    <G9,Sg> => "mu" ;
    <G9,Pl> => "" ;
    <G10,Sg> => "ku" ;
    <G10,Pl> => "ma" 
       } ;

    VowelAdjprefix: Cgender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Sg> => "mw" ;
    <G1,Pl> => "a" ;
    <G2,Sg> => "mw" ;
    <G2,Pl> => "mi" ;
    <G3,Sg> => "y" ;
    <G3,Pl> => "ma" ;
    <G4,Sg> => "ky" ;
    <G4,Pl> => "sy" ;
    <G5,Sg> => "ka" ;
    <G5,Pl> => "tw" ;
    <G6,Sg> => "va" ;
    <G6,Pl> => "kw" ;
    <G7,n> => "sy" ;
    <G8,Sg> => "mw" ;
    <_,_> => "ma" 
       } ;
   Withprefix : Number -> Cgender ->   Str = \n,g ->
   case <n,g> of {
  <Sg,G1> => "e" ;
  <Sg,G4> => "ki" ;
  <Sg,G5> => "ke" ;
  <Pl,G5> => "twi" ;
  <Sg,G6> => "ve" ;
  <Pl,G6>|<Sg,G10> => "kwi" ;
  <Pl,G4> |<Pl,G7>|<Pl,G9>=> "syi" ;
  <Sg,G3> |<Pl,G2>|<Sg,G7>=> "yi" ;
  <Sg,G2>|<Sg,G8> | <Sg,G9> => "wi" ;
  <Pl,G1> |<Pl,G3> |<Pl,G8> | <Pl,G10>=> "me" };
  PrefixPlNom : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  => "a" ;
    <G2> => "mi" ;
    <G4>  => "i" ;
    <G5> => "tu";
    <G6> => "ku" ;
    <G7> => [] ;
    <G9> => "mb" ;
    _ => "ma" 

          } ;
mkprefix,Ordprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> | <G2> | <G8> | <G9>  => "wa" ;
    <G3> | <G7> => "ya" ;
    <G4>   => "kya" ;
    <G5> => "ka";
    <G10> => "kwa";
    <G6> => "va"
          } ;
     Cardprefix : Cgender ->  Str = \g ->
   case <g> of {    
     <G1>|<G3> |<G10> => "a" ;
     <G2>| <G7>|<G4> |<G8> |<G9> => "i" ;
    <G5> => "tu" ;
    <G6> => "ku"
      } ;

Cardoneprefix  : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>|<G2> |<G8> |<G9>  => "u" ;
    <G4>   => "ki" ;
     <G10>   => "ku" ;
    <G3>  => "yi" ;
    <G5> => "ka" ;
    <G6> => "va";
    <G7>   => "i" 
      } ;
   Cardtwoprefix  : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>|<G3> |<G8> |<G10>  => "e" ;
    <G2> |<G4> | <G7> |<G9>  => "i" ;
    <G5> => "twi" ;
    <G6> => "kwi"
   
      } ;

    Detprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> | <G8>  => "a" ;
    <G2>   => "mi" ;
    <G3> | <G10>   => "ma" ;
    <G4> | <G7> | <G9>  => "" ;
    <G5> => "tu" ;
    <G6> => "ku"
   
      } ; 

 
---------------------------------------------

oper
  conjThan = "kwi" ;
  conjThat = "kuvita" ;
  such = "ota";
  that="uu";
  reflPron : Agr => Str = \\ag=> case ag of {
    Ag G1 Sg P1 => "nyie" ;
    Ag G1 Sg P2 => "we" ;
    Ag G1 Sg P3 => "we" ;
    Ag _  Sg P3 => "yo" ;
    Ag G1 Pl P1 => "ithyi" ;
    Ag G1 Pl P2 => "nyui" ;
    Ag G1 Pl P3 => "mo" ;
    Ag _  _  _  => "" 
    };

  superVery ="vyu";
  IDetprefixsg :Cgender ->  Str = \g ->
   case <g> of {    
    <G3 > |<G7> => "yi" ;
      <G4 > => "ki";
      <G5 > => "ke";
      <G6 > => "ve";
      <G10 > => "kwi";
      _ => "wi" }; 

 IDetprefixpl : Cgender ->  Str = \g ->
   case <g> of { <G5 > => "twi";
                   <G6 > => "kwi";
                   <G2>  => "yi" ;
                   <G4> | <G7> | <G9>=> "syi" ;
                   _ => "me"   };

param
   VForm =VPreProg | VInf |VPast |VPreDef | VGen |VExtension VExte ; 
   DForm = unit | teen | ten | hund ;
  AForm = AAdj Cgender Number | AComp Cgender Number | Advv;
oper

mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;


}
