instance DiffGus of DiffBantu  =  open CommonBantu, Prelude  in {

param 
  GenderGus = G1 | G2 | G3 |G4| G5|G6|G7|G8|G9|G10 ;
oper 
  Gender = GenderGus ;
  firstGender = G1 ; secondGender = G2 ;
  conjThan = "kobua" ;
  conjThat = "kobua" ;

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
 possess_Prepof,mkPrepof : Number => Gender => Str = 
    table Number {  Sg => table {
                    G1| G2  => "bwo" ;
                    G3 => "ya";
                    G4 => "ria";
                    G5 => "kia"; --
                    G6 => "rwa";
                    G7 => "ka";
                    G8 => "bwa";
                    G9 => "kwa";
                    G10 => "a"
                   }; 
                                 
                   Pl => table { G1 => "ba" ;
                    G2  => "ya" ;
                    G3|G6  => "cia";
                    G4  |G8|G9|G10 => "a";
                    G5  => "bi"; --
                    G7 => "bia"} } ;

   
 superVery ="bi";

Cardoneprefix  : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"oya";
    <G7> =>"aka";
    <G8>  =>"obo";
    <G4> =>"eri";
    <G3>  =>"eye";
    <G6> =>"oro";
    <G2>  =>"oyo";
    <G5> =>"eke";
    <G9> =>"oko";
    <G10> => "a"
      } ;
Cardtwoprefix  : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"ba";
    <G7> |<G5> =>"bi";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"i";
    <G2> |<G9>  =>"e";
    < G10> => ""
      } ;

  Allpredetprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1>   => "b" ;
    <G2>   => "y" ;
    <G3> |<G5>| <G8> => "bi" ;
    <G9> | <G10> |<G4>    => "" ;
    <G6> | <G7> => "ci" 
             } ;
        

  PrefixPlNom : Gender ->  Str = \g ->
   case <g> of {    
    <G1>  => "aba" ;
    <G2> => "eme" ;
    <G3> |<G6> => "ci" ;
    <G4>| <G8> |<G9>  => "ama" ;
    <G5> |<G7> => "ebi";
        <G10> => "" 
          } ;

  mkprefix,Ordprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"o";
    <G4> => "ria";
    <G5>  => "kia";
    <G3> => "ya";
    <G6>=> "rwa";
    <G7>=> "ka";
    <G8>=> "bwa";
    <G9>=> "kwa";
    < G10> => "a" 
          } ;

  Cardprefix : Gender ->  Str = \g ->
   case <g> of {    
     <G1>  =>"ba";
    <G7> |<G5> =>"bi";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"i";
    <G2> |<G9>  =>"e";
    <G10> => ""
      } ;

       Mostpredetprefix : Gender ->  Str = \g -> ""; -- not taken care of 
   
      ConsonantAdjprefix: Gender -> Number ->   Str = \n,g -> ""; --not taken care of
   {-case <n,g> of {
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
       } ; -}

    VowelAdjprefix: Gender -> Number ->   Str = \n,g ->""; -- not taken care of 
  {-} case <n,g> of {
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
       } ; -}

  VoweliAdjprefix: Gender -> Number ->   Str = \n,g -> ""; -- not taken care of 
  {-} case <n,g> of {
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
       } ; -}
Adjpprefix : Gender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Pl> => "aba" ;
    <G2,Pl> => "eme" ;
    <G3,Sg> => "e" ;
    <G4,Sg> => "eri" ;
    <G5,Sg> => "ege" ;
    <G5,Pl> => "ebi" ;
    <G6,Sg> => "oro" ;
    <G7,Sg> => "aka" ;
    <G7,Pl> => "ebi";
    <G8,Sg> => "obo" ;
     <G9,Sg> => "oko" ;
     <G10,Sg> => "aa" ;
     <G1,Sg>|<G2,Sg> => "omo" ;
     <G3,Pl> |<G6,Pl> => "ci" ;
     <G4,Pl>|<G8,Pl> |<G9,Pl> => "ama" ;
    <G10,Pl> => ""  
       } ;
  ProunSgprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"o";
    <G4> => "ria";
    <G5>  => "kia";
    <G3> => "ya";
    <G6>=> "rwa";
    <G7>=> "ka";
    <G8>=> "bwa";
    <G9>=> "kwa";
    <G10> => "a" 
          } ;

ProunPlprefix : Gender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> =>"ba";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"chia";
    <G2>  =>"ya";
    <G5> =>"bia";
    <_> => ""
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
