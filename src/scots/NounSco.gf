concrete NounSco of Noun = NounEng - [IndefArt] ** open Prelude, ResSco in {

lin IndefArt = {
      s = \\hasCard,n => case <n,hasCard> of {
        <Sg,False> => "a" ;
        _          => []
        } ;
      sp = \\g,hasCard,n => case <n,hasCard> of {
        <Sg,False> => table {NCase Gen => "ane's"; _ => "ane" };
        <Pl,False> => table {NCase Gen => "anes'"; _ => "anes" } ;
        _          => \\c => []
        } ;
      isDef = False
      } ;

}
