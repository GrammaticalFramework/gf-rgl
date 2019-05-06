--# -path=.:../prelude:../abstract:../common

concrete RelativeCgg of Relative = CatCgg ** open Prelude, ResCgg in {

lin
{-
	--IdRP  : RP ;                      -- which

	Apparently IdRP means Identity Relative Pronoun i.e. the most atomic part
	of a relative pronoun. The abstract syntax seems to alude that more 
	Relative pronouns can be formed when such a relative subject or object marker is affixed to 
	a prepositional Phrase.
	In Runynakore and Rukiga, relative pronouns depend on
	Noun Class , Gender  and the case of the noun they 
	refer to.

	Since this involves a table of two tables, we use ResCgg to prepare all possibilities


-}

	IdRP = {s = mkRPs; rObjVariant2 = mkRObjV2};        --: RP ;                      -- which
	--RelCl    : Cl -> RCl ;            -- such that John loves her
	-- The simplest way to form a relative clause is from a clause by
	-- a pronoun similar to "such that".
	RelCl cl = { 
      s = "kugira ngu" ++ cl.s ; 
      subjAgr = AgrYes cl.subjAgr;
      rp = mkRPs;
      rObjVariant2 = mkRObjV2;
      root = cl.root;
      morphs = cl.morphs;
      compl =cl.compl;
      isCompApStem = False;
      agr = AgrNo                              
      }; -- such that John loves her. why does it need any case?

	-- The more proper ways are from a verb phrase 
	-- (formed in [``Verb`` Verb.html]) or a sentence 
	-- with a missing noun phrase (formed in [``Sentence`` Sentence.html]).
	--RelVP    : RP -> VP -> RCl ;      -- who loves John
	
	RelVP rp vp =
	{ 
      s = []; 
      subjAgr = AgrNo; 
      rp = rp.s;
      rObjVariant2 = rp.rObjVariant2;
      root = vp.s;
      morphs = vp.morphs;
      compl =vp.comp;
      isCompApStem = vp.isCompApStem;
      agr = vp.agr                            
      };
	
      --RelSlash : RP -> ClSlash -> RCl ; -- whom John loves


	{-
--1 Relative clauses and pronouns

abstract Relative = Cat ** {

  fun

-- The simplest way to form a relative clause is from a clause by
-- a pronoun similar to "such that".

    RelCl    : Cl -> RCl ;            -- such that John loves her

-- The more proper ways are from a verb phrase 
-- (formed in [``Verb`` Verb.html]) or a sentence 
-- with a missing noun phrase (formed in [``Sentence`` Sentence.html]).

    RelVP    : RP -> VP -> RCl ;      -- who loves John
    RelSlash : RP -> ClSlash -> RCl ; -- whom John loves

-- Relative pronouns are formed from an 'identity element' by prefixing
-- or suffixing (depending on language) prepositional phrases or genitives.

    IdRP  : RP ;                      -- which
    FunRP : Prep -> NP -> RP -> RP ;  -- the mother of whom

-}

}
