instance DiffKam of DiffBantu = open CommonBantu, Prelude in {

param 
  GenderKam = G1 | G2 | G3 | G4 | G5 | G6 | G7| G8 | G9 |G10 ;
oper 
  Gender = GenderKam ;
  firstGender = G1 ; secondGender = G2 ;

  conjGender : Gender -> Gender -> Gender = \m,n -> 
    case m of { G1 => n ; _ => G2 } ;

  ProunSgprefix : Gender ->  Str = \g ->  
    case g of {    
      G1 | G2|G8 |G9 => "w" ;
      G3 | G7 => "y" ;
      G4      => "ky" ;
      G5      => "k";
      G10      => "kw";
      G6      => "v"
    } ;

  ProunPlprefix : Gender ->  Str = \g ->
    case g of {    
      G1 | G3 |G8 |G10 => "m" ;
      G2      => "y" ;
      G4 | G7 |G9 => "sy" ;
      G5      => "tw" ;
      G6      => "kw"
    } ;
Allpredetprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G3> |<G8> |<G10> => "" ;
    <G2>   => "y" ;
    <G4> | <G7> | <G9>  => "sy" ;
    <G5> => "two" ;
    <G6> => "kwo"
         } ;
 Mostpredetprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1>   => "ala a" ;
    <G2>   => "ila m" ;
    <G3> | <G10>  => "ala  ma" ;
    <G4> | <G8>   => "ila mb" ;
    <G5> => "tula twi" ;
    <G6> => "kula kwi" ;
    <G7> | <G9>  => "ila mb" 
   
      } ;
ConsonantAdjprefix: Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
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

    VowelAdjprefix: Gender -> Number ->   Str = \n,g ->
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
  Adjpprefix : Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Sg> => "wi" ;
    <G1,Pl> => "me" ;
    <G2,Sg> => "wi" ;
    <G2,Pl> => "yi" ;
    <G3,Sg> => "yi" ;
    <G3,Pl> => "me" ;
    <G4,Sg> => "ki" ;
    <G4,Pl> => "syi" ;
    <G5,Sg> => "ke" ;
    <G5,Pl> => "twi" ;
    <G6,Sg> => "ve" ;
    <G6,Pl> => "kwi" ;
    <G7,Sg> => "yi" ;
    <G7,Pl> => "syi";
    <_,_> => ""  
       } ;

  PrefixPlNom : Gender ->  Str = \g ->
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
mkprefix,Ordprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> | <G2> | <G8> | <G9>  => "wa" ;
    <G3> | <G7> => "ya" ;
    <G4>   => "kya" ;
    <G5> => "ka";
    <G10> => "kwa";
    <G6> => "va"
          } ;
     Cardprefix : Gender ->  Str = \g ->
   case <g> of {    
     <G1>|<G3> |<G10> => "a" ;
     <G2>| <G7>|<G4> |<G8> |<G9> => "i" ;
    <G5> => "tu" ;
    <G6> => "ku"
      } ;

Cardoneprefix  : Gender ->  Str = \g ->
   case <g> of {    
    <G1>|<G2> |<G8> |<G9>  => "u" ;
    <G4>   => "ki" ;
     <G10>   => "ku" ;
    <G3>  => "yi" ;
    <G5> => "ka" ;
    <G6> => "va";
    <G7>   => "i" 
      } ;
   Cardtwoprefix  : Gender ->  Str = \g ->
   case <g> of {    
    <G1>|<G3> |<G8> |<G10>  => "e" ;
    <G2> |<G4> | <G7> |<G9>  => "i" ;
    <G5> => "twi" ;
    <G6> => "kwi"
   
      } ;

    Detprefix : Gender ->  Str = \g ->
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
  conjThan = "kuvita" ;
  conjThat = "kuvita" ;

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

param
  VForm = VInf 
    | VPres Gender Number Person
    | VPast Gender Number Person
    | VFut  Gender Number Person
   -- | notpresent 
    ;
   
  DForm = unit | teen | ten | hund ;
  AForm = AAdj Gender Number | AComp Gender Number ;
}
