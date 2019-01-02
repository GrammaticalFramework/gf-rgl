resource MorphoAra = ResAra ** open Prelude in  {

flags optimize = all ;--noexpand;
  coding=utf8 ;

  oper

    mkDet = overload {
      mkDet : Str -> Number -> State -> Det
        = mkDetDecl True ;
      mkDet : (m,f : Str) -> Number -> State -> Det 
        = \m,f,n,d ->
        let detM = mkDetDecl True m n d ;
            detF = mkDetDecl True f n d ;
         in detM ** {
              s = \\h,g,c => case g of {
                        Fem  => detF.s ! h ! g ! c ;
                        Masc => detM.s ! h ! g ! c }
              } 
    } ;

    mkDetDecl : Bool -> Str -> Number -> State -> Det
      = \decl,word,num,state -> baseQuant **
      { s = \\_,_,c => word + if_then_Str decl (caseTbl ! c) [] ;
        n = numberToSize num;
        d = state;  --only Const is used now. check StructuralAra
      } ;

    mkPredet : Str -> Bool -> Predet
      = \word,decl ->
      { s = \\c =>
          case decl of {
            True => word + caseTbl!c;
            False => word
          };
        isDecl = decl
      };

   mkQuantNum : Str -> Number -> State -> {
      s: Species => Gender => Case => Str; n: Number; d : State; isPron: Bool; isNum : Bool} =
      \waHid,num,state ->
     let waHida = waHid + "َة" in
      { s = \\_,g,c =>
          let word =
          case g of {
            Masc => waHid;
            Fem => waHida
          } in defArt state c waHid + word + dec1sg ! state ! c;
        n = num;
        d = state;
        isPron = False;
        isNum = True
      };

}
