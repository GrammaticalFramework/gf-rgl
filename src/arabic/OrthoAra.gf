resource OrthoAra = open Prelude, Predef in {

flags coding=utf8 ;

  oper

    vow : pattern Str = #("َ" | "ِ" | "ُ" | "ً" | "ٍ" | "ٌ") ;

    weak : pattern Str = #("و"|"ي") ;

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

    rectifyHmz: Str -> Str = \word ->
      case word of {
        l@(""|"ال") + ("أ"|"أَ") + #hamza + "ْ" + tail => l + "آ" + tail;
        l@(""|"ال") + ("أ"|"أَ") + #hamza  + tail => l + "آ" + tail;
        l@(""|"ال") + #hamza + v@("َ"|"ُ") + tail => l + "أ" + v + tail;
        l@(""|"ال") + #hamza + v@("ِ")     + tail => l + "إ" + v + tail;

        head + v1@("ِ"|"ُ"|"َ"|"ْ"|"ا"|"ي"|"و") + #hamza + v2@(""|"ُ"|"َ"|"ْ"|"ِ") => head + v1 + (tHmz v1) + v2;
        head + #hamza + tail => head + (bHmz (dp 2 head) (take 2 tail)) + tail; --last head , take 1 tail
        _           => word
      };

    --hamza at beginning of word (head)
    hHmz : Str -> Str = \d ->
      case d of {
        "ِ" => "إ";
	    _	 => "أ"
      };

    --hamza in middle of word (body)
    bHmz : Str -> Str -> Str = \d1,d2 ->
      case <d1,d2> of {
        <"ِ",_> | <_,"ِ"> => "ئ";
	    <"ُ",_> | <_,"ُ"> => "ؤ";
	    <"َ",_> | <_,"َ"> => "أ";
	    _				   => "ء"
      };

    --hamza carrier sequence
    tHmz : Str -> Str = \d ->
      case d of {
        "ِ" => "ئ";
	    "ُ" => "ؤ";
	    "َ" => "أ";
	    "ْ"|"ا"|"و"|"ي" => "ء"
      };

}
