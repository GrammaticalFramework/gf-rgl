abstract DictFinAbs =
  OldDictFinAbs,
  NewDictFinAbs ;

-- AR 2018-08-23
-- Importing DictFin is not recommended, because
-- it duplicates most words.
-- Most users should use NewDictFin.
-- OldDictFin was called DictFin before,
-- so it is included here for backward compatibility.
-- Old
--  - generates Kotus tables with minimal form sets.
--  - replaces ä by a' etc in function names
--  - comments out derived adverbs
-- New
-- - generates standard RGL categories
--  - uses unicode ä,ö but must escape š with s'
--  - includes derived adverbs

