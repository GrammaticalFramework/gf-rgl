instance DiffGus of DiffBantu  =  open CommonBantu, Prelude  in {

param 
  CGenderGus = G1 | G2 | G3 |G4| G5|G6|G7|G8|G9|G10 | G11 ;
oper 
  Cgender = CGenderGus ;
  firstGender = G1 ; secondGender = G2 ;
  conjThan = "kobua" ;
  conjThat = "kobua" ;
  such =""; -- fill later
  kuna ="";
  that ="ng'a";
   conjGender : Cgender -> Cgender -> Cgender = \m,n -> 
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
 possess_Prepof,mkPrepof : Number => Cgender => Str = 
    table Number {  Sg => table {
                    G1| G2  => "bwo" ;
                    G3 => "ya";
                    G4 => "ria";
                    G5 => "kia"; --
                    G6 => "rwa";
                    G7 => "ka";
                    G8 => "bwa";
                    G9 => "kwa";
                    G10 => "a";
                    G11 => ""
                   }; 
                                 
                   Pl => table { G1 => "ba" ;
                    G2  => "ya" ;
                    G11 => "aa";
                    G3|G6  => "cia";
                    G4  |G8|G9|G10 => "a";
                    G5  => "bi"; --
                    G7 => "bia"} } ;

   
 superVery ="bi";

Cardoneprefix  : Cgender ->  Str = \g ->
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
    <G10> => "a" ;
    <G11> => ""
      } ;
Cardtwoprefix  : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  =>"ba";
    <G7> |<G5> =>"bi";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"i";
    <G2> |<G9>  =>"e";
    < G10> |<G11> => ""
      } ;

  Allpredetprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>   => "b" ;
    <G2>   => "y" ;
    <G5>| <G8> => "bi" ;
    <G9> | <G10> |<G4> |<G11>     => "" ;
    <G3> |<G6> | <G7> => "chi" 
             } ;
        

  PrefixPlNom : Cgender ->  Str = \g ->
   case <g> of {    
    <G1>  => "aba" ;
    <G2> => "eme" ;
    <G3> |<G6> => "chi" ;
    <G4>| <G8> |<G9>  => "ama" ;
    <G5> |<G7> => "ebi";
        <G10> |<G11> => "" 
          } ;

  mkprefix,Ordprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"o";
    <G4> => "ria";
    <G5>  => "kia";
    <G3> => "ya";
    <G6>=> "rwa";
    <G7>=> "ka";
    <G8>=> "bwa";
    <G9>=> "kwa";
    < G10> => "a" ;
    < G11> => ""
          } ;
  PrefixHowMany : Cgender ->  Str = \g ->
   case <g> of {    
     <G1>  =>"ba";
     <G6> =>"chi";
     <G3>  =>"i";
    <G2>  =>"e";
    <G7> |<G5> =>"bi";
    <G8> |<G4> |<G9> |<G10> |<G11>  =>"a"
          } ;

  Cardprefix : Cgender ->  Str = \g ->
   case <g> of {    
     <G1>  =>"ba";
    <G7> |<G5> =>"bi";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"i";
    <G2> |<G9>  =>"e";
    <G10> |<G11> => ""
      } ;

       
ConsonantAdjprefix : Cgender -> Number ->   Str = \n,g ->
   case <n,g> of {
    <G1,Pl> => "aba" ;
    <G2,Pl> => "eme" ;
    <G3,Sg> => "e" ;
    <G4,Sg> => "ri" ;
    <G5,Sg> => "ege" ;
    <G5,Pl> => "ebi" ;
    <G6,Sg> => "oro" ;
    <G7,Sg> => "aka" ;
    <G7,Pl> => "ebi";
    <G8,Sg> => "obo" ;
     <G9,Sg> => "oko" ;
     <G10,Sg> => "aa" ;
     <G1,Sg>|<G2,Sg> => "omo" ;
     <G3,Pl> |<G6,Pl> => "chi" ;
     <G4,Pl>|<G8,Pl> |<G9,Pl> |<G11,Pl>|<G11,Sg> => "ama" ;
    <G10,Pl>  => ""  
       } ;
  ProunSgprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G2> =>"o";
    <G4> => "ria";
    <G5>  => "kia";
    <G3> => "ya";
    <G6>=> "rwa";
    <G7>=> "ka";
    <G8>=> "bwa";
    <G9>=> "kwa";
    <G10> => "a";
    <G11> => "aa" 
          } ;

ProunPlprefix : Cgender ->  Str = \g ->
   case <g> of {    
    <G1> |<G7> =>"ba";
    <G8> |<G4> =>"a";
    <G3> |<G6> =>"chia";
    <G2>  =>"ya";
    <G5> =>"bia";
    <G11> => "aa" ;
    <_> => ""
         } ;

      dfltGender : Cgender = G1 ;
      dflt2Gender : Cgender = G2 ;
extension :VExte -> Str=\type ->case type of{
           EPassive => "gw" ;
           EApplicative => "er" ; -- note e
           EReciprocal  => "en" ;-- note also lew
           EStative  => "ek" ;
           ECausative => "ika"  --note alsolik/ek/uk
        };


 param
  VExte =  EPassive | EApplicative  |EReciprocal |EStative | ECausative ;
  VForm =  Vanter | VInf | VGen |VNeg |VExtension VExte |VFut;
  DForm = unit | teen | ten |hund  ;
  AForm = AAdj Cgender Number |Advv;
}
