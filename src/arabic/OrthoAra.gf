resource OrthoAra = open Prelude, Predef in {

flags coding=utf8 ;

oper

  vow : pattern Str = #("َ" | "ِ" | "ُ" | "ً" | "ٍ" | "ٌ") ;

  vstar : pattern Str = #("َ"|"ِ"|"ُ"|"ً"|"ٍ"|"ٌ"|"ْ"|"ا"|"ي"|"و") ; -- long or short vowels

  astar : pattern Str = #("َ"|"ً"|"ا") ; -- a: short, nunated or long
  istar : pattern Str = #("ِ"|"ي"|"يْ") ; -- i: short, long or long with sukun
  ustar : pattern Str = #("ُ"|"و"|"وْ") ; -- u: short, long or long with sukun

  weak : pattern Str = #("و"|"ي") ;

  hamzaseat : pattern Str = #("أ"|"ؤ"|"ئ") ;

  -- "Sun letters": assimilate with def. article
  sun : pattern Str = #("ت"|"ث"|"د"|"ذ"|"ر"|"ز"|"س"|"ش"|"ص"|"ض"|"ط"|"ظ"|"ل"|"ن") ;

-- Shadda: https://www.unicode.org/L2/L2017/17253-arabic-ordering.pdf
  fixShd : Str -> Str -> Str = \word,suffix ->
    case <word,suffix> of {
     -- <x + "ّ", v@#vow + y> => x + v + "ّ" + y ;
      <x + v@#vow, "ّ" + y> => x + "ّ" + v + y ;
      _                     => word + suffix
    } ;

-- IL: using this to reuse patterns for weak verbs, might be strange/wrong
  rmSukun : Str -> Str = \s -> case s of {
    x + "ْ" + y => x + y ;
    _           => s
  } ;

-- Hamza
  hamza : pattern Str = #("ء"|"؟") ;

  rectifyHmz : Str -> Str = \word ->
    case word of {
      l@(""|"ل"|"ال") + ("أ"|"أَ") + #hamza + "ْ" + tail => l + "آ" + tail;
      l@(""|"ل"|"ال") + ("أ"|"أَ") + #hamza  + tail      => l + "آ" + tail;
      l@(""|"ال") + #hamza + v@("َ"|"ُ") + tail      => l + "أ" + v + tail;
      l@(""|"ال") + #hamza + v@("ِ")     + tail      => l + "إ" + v + tail;
      head + v1@#vstar 
           + #hamza + v2@(#vow|"ْ") + tail => 
              case v2 of { "ْ" => head + v1 + bHmz v1 v2      + tail ; -- unsure about this /IL
                           _   => head + v1 + bHmz v1 v2 + v2 + tail } ;
      head + v1@#vstar -- the same but it ends in vowel
           + #hamza + v2@(#vow|"ْ") =>
              case v2 of { "ْ" => head + v1 + tHmz v1 ;
                           _   => head + v1 + tHmz v1 + v2 } ;
      head + v1@#vstar -- the same but it ends without vowel
           + #hamza => head + v1 + tHmz v1 ;

      head + #hamza + tail     => head + (bHmz (dp 2 head) (take 2 tail)) + tail; --last head , take 1 tail
      _                        => word
    };

  --hamza at beginning of word (head)
  hHmz : Str -> Str = \d ->
    case d of {
      "ِ" => "إ";
    _	 => "أ"
    };

  --hamza in middle of word (body)
  -- relaxing the pattern matching, so that we can call it from ResAra.sing /IL
  bHmz : Str -> Str -> Str = \d1,d2 ->
    case <d1,d2> of {
      <_+#istar,_> | <_,#istar> => "ئ";
      <_+#ustar,_> | <_,#ustar> => "ؤ";
      <_+"َ" ,_>   | <_,"َ">  => "أ"; -- #astar would allow double alif
      _                       => "ء"
    };

  --hamza carrier sequence
  tHmz : Str -> Str = \d ->
    case d of {
      "ِ" => "ئ";
      "ُ" => "ؤ";
      "َ" => "أ";
      _   => "ء" -- long vowels and sukun
    };

}
