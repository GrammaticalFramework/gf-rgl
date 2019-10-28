concrete ExtraKam of ExtraKamAbs = ExtraBantuKam ** 
open CommonBantu,ParadigmsKam,MorphoKam, ResKam in {
lin
that_far_Quant = let
      questo : ParadigmsKam.Number =>  MorphoKam.Cgender =>  Str = table {
    Sg => \\g=> case <g> of {
                    <G3 > => "yiu";
                    <G4 > => "kyu";
                    <G5 > => "kau";
                    <G10 > => "kuu";
                    <G6 > => "vau";
                    <G7 >  => "isu" ;
                    _ => "usu"}; 
                       
      Pl => \\g=> case <g> of{
                    <G5 > => "tuu";
                    <G6 > => "ku";
                    <G7> |<G9> |<G2> |<G4> => "isu";
                    _ => "asu" } }
    in { s = questo ; } ;

}
