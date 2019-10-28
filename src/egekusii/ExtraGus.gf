concrete ExtraGus of ExtraGusAbs = ExtraBantuGus ** 
open CommonBantu,ParadigmsGus,MorphoGus, ResGus in {
lin
that_far_Quant = let
      questo : Number =>  MorphoGus.Nclass =>  Str = table {
    Sg => \\g=> case <g> of {
                  <G1> => "ouio";
                  <G2> => "oyio";
                  <G4> => "erio";
                  <G5> => "ekio";
                  <G3> => "eyio";
                  <G6> => "oruo";
                  <G7> => "akuo";
                  <G8> => "abuo";
                  <G9> => "oku";
                  <G11> "";
                  <G10>  => "abou"
                   }; 
                       
      Pl => \\g=> case <g> of{
                  <G1> => "abuo";
                  <G2> => "eyio";
                  <G4> | <G8> => "ayio"; --confirm gendder 8
                  <G5> => "ebio";
                  <G3> | <G6> => "ecio";
                  <G7> => "ebio";
                  <G9> => "eyio";
                  <G11> => "eyio";
                  <G10> => "ayio" } }
    in { s = questo ; } ;

}
