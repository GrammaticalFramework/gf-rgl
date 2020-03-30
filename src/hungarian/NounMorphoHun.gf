resource NounMorphoHun = ParamHun ** open Prelude, Predef in {

oper
  Noun = {s : Number => Case => Str} ;

  -- Paradigm functions
  -- http://www.cse.chalmers.se/~aarne/articles/smart-preprint.pdf

  -- Words like alma, kefe:
  dAlma : Str -> Noun = \alma ->
    let almá : Str = lengthen alma ;

        -- Apply mkNoun to the lengthened stem "almá" or "kefé"
        nAlmá : Noun = mkNoun almá ;
     in {s = \\n,c => case <n,c> of {
                -- Singular nominative uses the given form, e.g. "alma" or "kefe"
                <Sg,Nom> => alma ;

                -- The rest of the forms are formed with the regular constructor,
                -- using "almá" or "kefé" as the stem.
                _ => nAlmá.s ! n ! c
              } ;
        } ;

  -- Handles words like "madár", "név" with shortened stem vowel in plural
  dMadár : Str -> Noun = \madár ->
    let r = last madár ;
        madá = init madár ;
        mada = shorten madá ; -- shortens vowels
        a = last mada ;
        madara = mada + r + a ;
        nMadara = mkNoun madara ;
        nMadár = mkNoun madár ;
    in {s = \\n,c => case <n,c> of {
                -- All plural forms and Sg Acc use the "madara"/"neve" stem
                <Pl,_>|<Sg,Acc> => nMadara.s ! n ! c ;

                -- The rest of the forms are formed with the regular constructor,
                -- using "madár"/"név" as the stem.
                _ => nMadár.s ! n ! c

              } ;
        } ;

  -- TODO: actual paradigm
  dSör : Str -> Noun = \sör ->
    let foo : Str = "foo" ;
     in mkNoun sör ;

  -- More words not covered by current paradigms:
  -- https://cl.lingfil.uu.se/~bea/publ/megyesi-hungarian.pdf
  -- falu ~ falva-k
  -- gyomor ~ gyomr-ot
  -- sátor ~ satr-at
  -- TODO: do we need possessive forms? e.g. fiú ~ fia{m,d,tok}

  -- regNoun is a /smart paradigm/: it takes one or a couple of forms,
  -- and decides which (non-smart) paradigm is the most likely to match.
  regNoun : Str -> Noun = \sgnom -> case sgnom of {
    _ + "a"|"e"     => dAlma sgnom ;
    _ + ("á"|"é") + ? => dMadár sgnom ;

    -- TODO: more non-smart paradigms + more pattern matching
    -- TODO: smart paradigms with >1 form. Which forms are the most descriptive?

    _ => mkNoun sgnom -- Fall back to the regular paradigm
  } ;

--------------------------------------------------------------------------------
-- Following code by EG in 2009 (?), comments and some additions by IL 2020

param
  -- Harmony types
  Harm = H_a | H_e | H_o ;

oper

  -- Vowels as a pattern.
  v : pattern Str = #("a" | "e" | "i" | "o" | "u" | "ö" | "ü" |
                      "á" | "é" | "í" | "ó" | "ú" | "ő" | "ű") ;

  -- Function to test if a string ends in a vowel
  vowFinal : Str -> Bool = \str ->
    case str of {
      _ + #v => True ; -- Matching a string against a pattern.
      _ => False
    } ;

  lengthen : Str -> Str = \str -> case str of {
    x + "a" => x + "á" ;
    x + "e" => x + "é" ;
    x + "i" => x + "í" ;
    x + "o" => x + "ó" ;
    x + "u" => x + "ú" ;
    x + "ö" => x + "ő" ;
    x + "ü" => x + "ű" ;
    _   => Predef.error "Lengthening not applicable to" ++ str
  } ;

  shorten : Str -> Str = \str -> case str of {
    x + "á" => x + "a" ;
    x + "é" => x + "e" ;
    x + "í" => x + "i" ;
    x + "ó" => x + "o" ;
    x + "ú" => x + "u" ;
    x + "ő" => x + "ö" ;
    x + "ű" => x + "ü" ;
    _   => Predef.error "Shortening not applicable to" ++ str
  } ;

  -- Function to get a harmony from a string
  getHarm : Str -> Harm = \s -> case s of {
    _ + ("a" | "á" | "o" | "ó" | "u" | "ú") + _ => H_a ;
    _ + ("ö" | "ő" | "ü") + _                   => H_o ;
    _ => H_e
    } ;

  -- Used as a table of allomorphs for a give case.
  HarmForms : Type = Harm => Str ;

  -- Functions for constructing a HarmForms table.
  harm3 : Str -> Str -> Str -> HarmForms = \a,e,o -> table {
    H_a => a ;
    H_e => e ;
    H_o => o
    } ;
  harm  : Str -> Str -> HarmForms = \a,e -> harm3 a e e ;
  harm1 : Str -> HarmForms = \i -> harm i i ;

  -- Variant of case forms when the noun stem ends in consonant.
  endCaseCons : Case -> HarmForms = \c -> case c of {
    Nom => harm1 [] ;
    Acc => harm3 "ot" "et" "öt" ;
    Dat => harm "nak" "nek" ;
    Ill => harm "ba" "be" ;
    Ine => harm "ban" "ben" ;
    Ela => harm "ból" "ből" ;
    All => harm3 "hoz" "hez" "höz" ;
    Ade => harm "nál" "nél" ;
    Abl => harm "tól" "től" ;
    Sub => harm "ra" "re" ;
    Sup => harm3 "on" "en" "ön" ;
    Del => harm "ról" "ről" ;
    Cau => harm1 "ért" ;
    Ins => harm "al" "el" ;
    Tra => harm "á" "é"
    -- Ess => harm "stul" "stül" ;  -- Essive-modal 'with <the noun> and its parts'
    -- Ter => harm1 "ig" ; -- Terminative 'as far as <the noun>'
    -- For => harm1 "ként" ; -- Formal 'as <the noun>'
    -- Tem => harm1 "kor" -- Temporal 'at <numeral>'. Only used with numerals.
    } ;

  -- Variant of case forms when the noun stem ends in vowel.
  endCaseVow : Case -> HarmForms = \c -> case c of {
    Acc => harm1 "t" ;
    Sup => harm1 "n" ;
    Ins => harm "val" "vel" ;
    Tra => harm "vá" "vé" ;

    -- Other forms are shared with endCaseCons.
    _   => endCaseCons c
  } ;

  -- Function to return a plural allomorph given the stem (e.g. nev, almá).
  pluralAllomorph : (stem : Str) -> Str = \stem ->
    case vowFinal stem of {
      True  => "k" ;
      False => harm3 "ok" "ek" "ök" ! getHarm stem
    } ;


  -- Harmony and plural allomorph read from the singular nominative
  mkNoun : Str -> Noun = \w ->
    mkNounHarm (getHarm w) (pluralAllomorph w) w ;

  -- Harmony and plural allomorph given explicitly
  mkNounHarm : Harm -> (plural : Str) -> Str -> Noun = \h,plural,w ->
    let endCase : Case -> HarmForms = case vowFinal w of {
                                        True  => endCaseVow ;
                                        False => endCaseCons } ;

        -- Last consonant doubles before instrumental and translative
        lastCons : Str = case vowFinal w of {
                           True  => [] ;
                           False => last w } ;

        -- Noun is {s : Number => Case => Str}, we construct nested tables.
     in {s = table {
               Sg => table {
                       -- Double the last letter (if consonant) before Ins, Tra
                       c@(Ins|Tra) => w + lastCons + endCase c ! h ;
                       c@_         => w +            endCase c ! h } ;

               Pl => table {
                       -- Double the plural k before Ins, Tra
                       c@(Ins|Tra) => w + plural + "k" + endCaseCons c ! h ;

                       -- endCaseCons, because we only use -k as plural morpheme.
                       -- If we add possessive forms with allomorph -i, then revise.
                       c@_         => w + plural +       endCaseCons c ! h }
             }
   } ;

}
