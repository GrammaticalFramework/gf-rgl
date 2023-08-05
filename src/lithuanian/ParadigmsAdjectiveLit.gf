--# -path=.:../prelude:../common
--# -coding=utf8

-- L.Boizou, 2022 <lboizou@gmail.com>

resource ParadigmsAdjectiveLit = ResLit ** open Prelude, (Predef=Predef), MorphoAdjectiveLit in {

     flags  coding=utf8; 
  

  oper mkComp : Str -> AdjForms
    = \adj -> 
      case adj of {
        s + ("ias" | "as" | "us" | "is" | "ys") => adj2aModel (s + "esn") ;
        _ => Predef.error ("Error (mkAdjComp): incorrect adjective form:" + adj)
      } ;
      
  oper mkSup : Str -> AdjForms 
    = \adj -> 
      case adj of {
        s + "ias" => adj1aModel (s + "iausi") ;
        s + ("as" | "us" | "is" | "ys") => adj1aModel ((soften s) + "iausi") ;
        _ => Predef.error ("Error (mkAdjSup): incorrect adjective form:" + adj)
      } ;

  oper mkAdvPos : Str -> Str 
    = \adj -> 
      case adj of {
        s + ("us" | "is" | "ys") => (soften s) + "iai" ;
        s + "as" => s + "ai" ;
        _ => Predef.error ("Error (mkAdvPos): incorrect adjective form:" + adj)
      } ;

  oper mkAdvComp : Str -> Str 
    = \adj -> 
      case adj of {
        s + "ias" => s + "iau" ;
        s + ("as" | "us" | "is" | "ys") => (soften s) + "iau" ;
        _ => Predef.error ("Error  (mkAdvComp): incorrect adjective form:" + adj)
      } ;

  -- didesnis -> didžiau
  oper mkAdvCompFromComp : Str -> Str 
    = \adj -> 
      case adj of {
        s + ("esnis") => s + "iau" ;
        _ => Predef.error ("Error (mkAdvCompFromComp): incorrect adjective form:" + adj)
      } ;

  oper mkAdvSup : Str -> Str 
    = \adj -> 
      case adj of {
        s + ("ias" | "as" | "us" | "is" | "ys") => s + "iausiai" ;
        _ => Predef.error ("Error (mkAdvSup): incorrect adjective form:" + adj)
      } ;


  -- oper for simple forms
  oper mkRegAdj = overload {
    mkRegAdj : Str -> Adj =
    \pos -> {
      pos = guessAdjModel pos;
      comp = mkComp pos;
      super = mkSup pos;
      advpos = mkAdvPos pos;
      advcomp = mkAdvComp pos;
      advsuper = mkAdvSup pos
    };
    mkRegAdj : Str -> Str -> Str -> Adj =
    \pos, comp, sup -> {
      pos = guessAdjModel pos;
      comp = guessAdjModel comp;
      super = guessAdjModel sup;
      advpos = mkAdvPos pos;
      advcomp = mkAdvCompFromComp comp;
      advsuper = mkAdvPos sup
    };
  };

-- Another option : right_Ord = { s = mkAtable (guessAdjModel "dešinys") }; 
-- A revoir : nonExist provoque une erreur
{-
  oper mkNoDegreeAdj : Str -> Adj =
    \pos -> {
      pos = guessAdjModel pos;
      comp = nonExist;
      super = nonExist;
      advpos = nonExist;
      advcomp = nonExist;
      advsuper = nonExist;
     };
-}  

  oper mkCompAdj = overload {
-- utilise par jaki, taki, de morphoPol...
-- a supprimer ou renommer
-- le nom ne convient pas...
    mkCompAdj : Str -> Adj =
    \pos -> {
      pos = guessAdjModel pos;
      comp = mkComp pos;
      super = mkSup pos;
      advpos = "["++pos ++ [": the adverb positive form does not exist]"];
      advcomp = "["++pos ++ [": the adverb comparative form does not exist]"];
      advsuper = "["++pos ++ [": the adverb superlative form does not exist]"]
    };
  };
  
  addComplToAdj : Adj -> Str -> Case -> (Adj ** { c:Complement });
  addComplToAdj a s c = { pos = a.pos; comp=a.comp; super=a.super;
    advpos=a.advpos; advcomp=a.advcomp; advsuper=a.advsuper;
    c = mkCompl s c
  };


}
