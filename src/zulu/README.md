# GF Zulu Resource Grammar

### Authors
- Laurette Marais (lmarais@csir.co.za)
- Laurette Pretorius (laurette.pretorius@gmail.org)

  in collaboration with Lionel Posthumus

This work has been made possible by the [South African Centre for Digital Language Resource (SADiLaR)](https://www.sadilar.org).

## Introduction
This resource grammar provides an implementation of the Zulu language by making use of the common abstract syntax and a custom extension. The goal is to strike a balance between adhering as closely as possible to the common abstract syntax, while providing as much transparency and efficiency as possible. One important aim is to enable parsing, which is not possible for large, inefficient grammars. The most common, and most challenging, syntax categories and functions have so far been implemented.

## Usage
The file structure has been adapted slightly to accommodate the custom extension, as well as to provide the ability to compile the base grammar with different lexica.

| Compilable module | Lexicon | Description |
| ----------------- | ------- | ----------- |
| MonoLexLangZul.gf | MonoLexZul.gf | The base RG with a large lexicon containing Zulu roots and stems, using Zulu function names, eg. `hamb_V` |
| MultiLexLangZul.gf | MultiLexZul.gf | The base RG with a lexicon of common Zulu roots and stems, using English function names, eg. `walk_V` |
| MonoLexChunkZul.gf | MonoLexZul.gf | A chunk parsing enabled version of the RG with a large lexicon containing Zulu roots and stems, using Zulu function names, eg. `hamb_V` |
| MultiLexChunkZul.gf | MultiLexZul.gf | A chunk parsing enabled version of the RG with a lexicon of common Zulu roots and stems, using English function names, eg. `walk_V` |
| DevLexLangZul.gf | DevLexZul.gf | The base RG with a small development lexicon containing Zulu roots and stems, using Zulu function names, eg. `hamb_V` |

Compiler directives setting up the appropriate paths have been added, so these modules should be compilable as is:

`path/to/gf-rgl$ gf --make src/zulu/grammars/MonoLexLangZul.gf`

## Supporting resources
Various supporting resources can be found at https://github.com/LauretteM/gf-zulu-resources
