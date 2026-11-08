-- Czech constructions beyond the portable Syntax API.
abstract ExtraCzeAbs = Cat ** {
fun
  -- Subject-oriented secondary adjective: mít rád (něco).
  -- The object remains open for ComplSlash or Extend.ReflRNP.
  SlashV2AP : V2 -> AP -> VPSlash ;
  -- First argument: dative experiencer. Second: nominative expression,
  -- which controls agreement: je mi pět let / jsou mi dva roky.
  DativeCopulaCl : NP -> NP -> Cl ;
  DativeCopulaQCl : NP -> IP -> QCl ;
}
