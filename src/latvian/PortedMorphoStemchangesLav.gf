--# -path=.:abstract:common:prelude

-- Here goes manually ported stemchanges from 
-- https://github.com/PeterisP/morphology/blob/master/src/main/java/lv/semti/morphology/analyzer/Mijas.java

resource PortedMorphoStemchangesLav = open Prelude, Predef, ResLav in {

flags coding = utf8 ;

oper

  stemchangeSimple : Int -> Str -> Str = \parId,stem ->
    case parId of {
      0 => stem ;

      -- For nouns we need changes 0, 1, 17
      1 => case stem of {  -- nouns
        s + ("kst") => s + "kš" ;
        s + ("nst") => s + "nš" ;
        s + ("ll")  => s + "ļļ" ;
        s + ("sl")  => s + "šļ" ;
        s + ("zl")  => s + "žļ" ;
        s + ("ln")  => s + "ļņ" ;
        s + ("nn")  => s + "ņņ" ;
        s + ("sn")  => s + "šņ" ;
        s + ("zn")  => s + "žņ" ;
        s + ("īt")  => s + "īš" ;
        s + ("d")   => s + "ž" ;
        s + ("t")   => s + "š" ;
        s + ("n")   => s + "ņ" ;
        s + ("s")   => s + "š" ;
        s + ("z")   => s + "ž" ;
        s + ("b"|"f"|"m"|"p"|"v") => stem + "j" ;
                   
        _ => stem
      } ;

      17 => case <stem, countSyllables stem > of { -- shortened vocative for fem nouns
        <s + ("īt"|"iņ"), _ > => stem ;
        --<_, 0|1>              => variants {} ;
        <_, 0|1>              => nonExist ; -- Exception
        <_, _>                => stem
      } ;

      -- For full adjectives we need changes 0, 3, 34, 13 
            
      _ => error ("Unsupported stemchange")
    };
    
  stemchangeForAdjAdv : Int -> Str -> Definiteness -> Degree -> Str = \parId,stem,defi,deg ->
    case parId of {
      3  => case stem of {
        s + "āk" => case deg of {
          Posit => stem ;
          _ => nonExist -- TODO what should go there for nonexisting forms?
        };
        _     => case deg of {
          Posit => stem ;
          Compar => stem + "āk" ;
          Superl => case defi of {
            Def   => "vis" + stem + "āk" ;
            Indef => nonExist 
          } 
        }
      } ;
      34 => case deg of { -- pēdēj-ais -> pēdē-jam, zaļ-š -> zaļa-jam
        Posit  => case stem of {
          s + "ēj" => s + "ē" ;
          _        => stem + "a" 
        } ;
        Compar => stem + "āka" ; 
        Superl => case defi of { 
          Def   => "vis" + stem + "āka" ;
          Indef => nonExist
        } 
      } ;

      _ => case deg of { _ => stemchangeSimple parId stem }
    } ;
    
    -- Inari's trick for counting syllables https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#cute-way-to-count-syllables
    -- pattern macro for vowels
    v : pattern Str = #("a"|"ā"|"e"|"ē"|"i"|"ī"|"o"|"u"|"ū") ;

    -- type alias for the helper function
    SylCnt : Type = Bool -> Str -> Ints 10 ;

    countSyllables : Str -> Ints 10 = go count False
      where { -- synonym of let
      go : SylCnt -> SylCnt = \f,wasVowel,word -> -- These three arguments come from the fact that SylCnt is the abovedefined 3rd order function.
        case <word,wasVowel> of {
          <#v + s, False> => Predef.plus (f True s) 1 ;
          <#v + s, True>  => f True s ;
          <?  + s, _>     => f False s ;
          _               => 0
        } ;

      -- end of recursion
      scBase : SylCnt = \_,_ -> 0 ;

      -- the function given to countSyllables
      count : SylCnt = go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go scBase)))))))))))))))))))))))))))))))))))))))
    } ;

}
