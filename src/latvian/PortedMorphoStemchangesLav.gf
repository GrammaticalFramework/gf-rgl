--# -path=.:abstract:common:prelude

-- Here goes manually ported stemchanges from 
-- https://github.com/PeterisP/morphology/blob/master/src/main/java/lv/semti/morphology/analyzer/Mijas.java

resource PortedMorphoStemchangesLav = open Prelude, Predef in {

flags coding = utf8 ;

oper

    stemchange : Int -> Str -> Str = \parId,stem ->
        case parId of {
            0 => stem;
            1 => case stem of { -- nouns
                    s + ("kst") => s + "kš";
                    s + ("nst") => s + "nš";
                    s + ("ll")  => s + "ļļ";
                    s + ("sl")  => s + "šļ";
                    s + ("zl")  => s + "žļ";
                    s + ("ln")  => s + "ļņ";
                    s + ("nn")  => s + "ņņ";
                    s + ("sn")  => s + "šņ";
                    s + ("zn")  => s + "žņ";
                    s + ("īt")  => s + "īš";
                    s + ("d")   => s + "ž";
                    s + ("t")   => s + "š";
                    s + ("n")   => s + "ņ";
                    s + ("s")   => s + "š";
                    s + ("z")   => s + "ž";
                    s + ("b"|"f"|"m"|"p"|"v") => stem + "j";
                    
                    _ => stem
                };

            17 => case <stem, countSyllables stem > of { -- shortened vocative for fem nouns
                    <s + ("īt"|"iņ"), _ > => stem;
                    --<_, 0|1>              => variants {};
                    <_, 0|1>              => nonExist; -- Exception
                    <_, _>                => stem
                };
            
            _ => error ("Unsupported stemchange")
        };
    

    
    -- Inari's trick for counting syllables https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#cute-way-to-count-syllables
    -- pattern macro for vowels
    v : pattern Str = #("a"|"ā"|"e"|"ē"|"i"|"ī"|"o"|"u"|"ū") ;

    -- type alias for the helper function
    SylCnt : Type = Bool -> Str -> Ints 10 ;

    countSyllables : Str -> Ints 10 = go count False
    where {
    go : SylCnt -> SylCnt = \f,wasVowel,word ->
        case <word,wasVowel> of {
        <#v + s, False> => Predef.plus (f True s) 1 ;
        <#v + s, True>  => f True s ;
        <?  + s, _>     => f False s ;
        _               => 0 } ;

    -- end of recursion
    scBase : SylCnt = \_,_ -> 0 ;

    -- the function given to countSyllables
    count : SylCnt = go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go (go scBase)))))))))))))))))))))))))))))))))))))))
    } ;

}
