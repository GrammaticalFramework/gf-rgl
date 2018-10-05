instance DiffKis of DiffBantu  =  open CommonBantu, Prelude  in {

param 
  GenderKis = G1|G2|G3|G4|G5|G6|G7|G8|G9|G10|G11|G12|G13  ;
oper 
  Gender = GenderKis ;
  firstGender = G1 ; secondGender = G2 ;
  conjThan = "kuliko" ;
  conjThat = "kuvita" ;

   conjGender : Gender -> Gender -> Gender = \m,n -> 
    case m of { G1 => n ; _ => G2 } ;
  reflPron :Agr => Str = \\ag=> case <ag >  of  {
         < Ag G1 Sg P1  >     => "mimi" ;
         < Ag G1 Sg P2  >     => "wewe" ;
         < Ag G1 Sg P3 >     => "yeye" ;
         < Ag _ Sg P3 >     => "" ;
         < Ag G1 Pl P1 >     => "sisi" ;
         < Ag G1 Pl P2  >     => "nyinyi" ;
         < Ag G1 Pl P3 >     => "wao" ;
          < Ag _ _ _ >     => "" 
        
         };
possess_Prepof,part_Prepof:Number => Gender => Str = 
    table Number { Sg => table {    G3=> "la" ; 
                                 G4 => "cha" ; 
                                 G5 => "ya" ; 
                                 G11 => "pa";
                                 G12 => "kwa";
                                 G13 => "mwa";
                                 G1 |G2|G6|G7 |G8 => "wa" ;
                                  _ => ""} ; 
                                 
                   Pl => table { G1 => "wa" ; 
                                 G4 => "vya" ; 
                                 G5|G6 => "za" ; 
                                 G2|G3 |G8 |G9 |G10 => "ya" ; 
                                 _ => ""} } ;
   
  
 superVery ="vyu";

Cardoneprefix  : Gender ->  Str = \g ->
   case <g> of {    
      <G4>   => "ki" ;
    <G1>|<G2> |<G6> |<G8>  => "m" ;
    _  => "" 
         } ;
Cardtwoprefix  : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  => "waw" ;
    <G2>   => "miw" ;
    <G3> |<G8>   => "maw" ;
    <G4>   => "viw" ;
    _=> "mb" 
          } ;
  Allpredetprefix : Gender ->  Str = \g ->
   case <g> of {    
   <G4>   => "vy" ;
    <G11> => "p";
    <G12> => "k";
    <G13> => "m";
    <G5> | <G6> => "z" ;
    <G1> |<G7>   => "w" ;
    <G2>|<G3> | <G8> |<G9>  |<G10>   => "y" 
         } ;
  PrefixPlNom : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  => "wa" ;
    <G2> => "mi" ;
    <G3>  => "ma" ;
    <G4>  => "vi" ;
         _ => [] 
          } ;
  mkprefix,Ordprefix : Gender ->  Str = \g ->
   case <g> of {    
   
    <G3>  => "la" ;
    <G4>   => "cha" ;
    <G5>| <G9> |<G10>  => "ya";
    <G11>| <G12> |<G13>  => "pa";
     <G1> | <G2> |<G6> |<G7> |<G8> => "wa" 
            } ;
  Cardprefix : Gender ->  Str = \g ->
   case <g> of {    
     <G1>  => "wa" ;
     <G2>  => "mi" ;
     <G3>|<G8>  => "ma" ;
    <G4> => "vi" ;
    _ => ""
      } ;
       Mostpredetprefix : Gender ->  Str = \g ->
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
      ConsonantAdjprefix: Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Sg> => "m" ;
    <G1,Pl> => "wa" ;
    <G2,Sg> => "m" ;
    <G2,Pl>  => "mi" ;
    <G3,Pl> => "ma" ;
    <G4,Sg> => "ki" ;
    <G4,Pl> => "vi" ;
    <G6,Sg>  => "m" ;
    <G7,_>  => "m" ;
    <G8,Sg>  => "m" ;
    <G8,Pl> => "ma" ;
    <G9,_> => "ma" ;
    <G11,Sg> => "pa" ;
    <G12,Sg> => "ku" ;
   <G13,Sg> => "m" ;
     <_,_> => "" 
       } ;

    VowelAdjprefix: Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Sg> => "mw" ;
    <G1,Pl> => "w" ;
    <G2,Sg> => "mw" ;
    <G2,Pl> => "my" ;    
    <G3,Sg> => "j" ;
    <G3,Pl> => "m" ; 
    <G4,Sg> => "ch" ;
    <G4,Pl> => "vy" ;
    <G5,Sg> => "ny";
    <G5,Pl> => "ny";
    <G6,Sg>=> "mw" ;
    <G6,Pl> => "y" ;
    <G7,Sg> => "mw" ;
    <G7,Pl> => "mw" ;
    <G8,Sg>=> "mw" ;
    <G8,Pl>  => "m" ; 
    <G9,_>  => "m" ;    
    <G10,_> => "ny" ;
    <G11,Sg> => "p" ;
    <G12,Sg> => "kw" ;
    <G13,Sg> => "mu" ;    
       <_,_> => "" 
       } ;

  VoweliAdjprefix: Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Sg> => "mwi" ;
    <G1,Pl> => "we" ;
    <G2,Sg> => "mwi" ;
    <G2,Pl> => "mi" ;    
    <G3,Sg> => "ji" ;
    <G3,Pl> => "me" ; 
    <G4,Sg> => "ki" ;
    <G4,Pl> => "vi" ;
    <G5,Sg> => "zi";
    <G5,Pl> => "zi";
    <G6,Sg>=> "mwi" ;
    <G6,Pl> => "zi" ;
    <G7,Sg> => "mwi" ;
    <G7,Pl> => "mwi" ;
    <G8,Sg>=> "mwi" ;
    <G8,Pl>  => "me" ; 
    <G9,_>  => "me" ;    
    <G10,_> => "zi" ;
    <G11,Sg> => "pe" ;
    <G12,Sg> => "kwi" ;
    <G13,Sg> => "mwi" ;    
       <_,_> => "" 
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
    <G7,Pl> => "syi" ;
    <_,_> => ""
       } ;
   ProunSgprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G3>  => "l" ;
    <G4>   => "ch" ;
    <G11> => "p";
    <G12> => "kw";
    <G13> => "mw";
    <G5> | <G9>| <G10>=> "y";
    _ => "w" 
    
          } ;

  ProunPlprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G4>   => "vy" ;
    <G11> => "p";
    <G12> => "kw";
    <G13> => "mw";
    <G5> | <G6> => "z" ;
    <G1> |<G7>   => "w" ;
    <G2>|<G3> | <G8> |<G9>  |<G10>   => "y" 
      
   
      } ;
      dfltGender : Gender = G1 ;
      dflt2Gender : Gender = G2 ;



 param

  VForm = VInf 
     | VPres Gender Number Person
    | VPast Gender Number Person
    | VFut  Gender Number Person
   -- | notpresent 
     ;
   
     DForm = unit | teen | ten |hund  ;
    AForm = AAdj Gender Number;
}
