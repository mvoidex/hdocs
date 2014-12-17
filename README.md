hdocs
=====

[![Hackage version](https://img.shields.io/hackage/v/hdocs.svg?style=flat)](http://hackage.haskell.org/package/hdocs) [![Build Status](https://travis-ci.org/mvoidex/hdocs.png)](https://travis-ci.org/mvoidex/hdocs)

Haskell docs util<br>
Unlike haskell-docs it can dump docs about module in JSON and allows '-g' option to configure GHC<br>
It can also dump all installed docs<br>

Ask it for documentation:
<pre>
$ hdocs Data.List all
Applied to a predicate and a list, all determines if all elements
 of the list satisfy the predicate. For the result to be
 True, the list must be finite; False, however, results from a False
 value for the predicate applied to an element at a finite index of a finite or infinite list.
$ hdocs docs Data.Either
{"either":"Case analysis for the Either type.\n If the value is `Left a`, apply the first function to `a`;\n if it is `Right b`, apply the second function to `b`.","partitionEithers":"Partitions a list of Either into two lists\n All the Left elements are extracted, in order, to the first\n component of the output. Similarly the Right elements are extracted\n to the second component of the output.","Either":"The Either type represents values with two possibilities: a value of\ntype `Either a b` is either `Left a` or `Right b`.\n\nThe Either type is sometimes used to represent a value which is\neither correct or an error; by convention, the Left constructor is\nused to hold an error value and the Right constructor is used to\nhold a correct value (mnemonic: \"right\" also means \"correct\").","rights":"Extracts from a list of Either all the Right elements\n All the Right elements are extracted in order.","lefts":"Extracts from a list of Either all the Left elements\n All the Left elements are extracted in order."}
$ (hdocs dump | json).'GHC.List'.'notElem'
notElem is the negation of elem.
$ (hdocs dump | json).'Data.List'.'null' # No reexports, 'null' is defined in GHC.List
$ (hdocs dump r | json).'Data.List'.'null' # 'r' — dump reexports too
Test whether a list is empty.
</pre>
