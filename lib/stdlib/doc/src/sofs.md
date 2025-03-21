Functions for manipulating sets of sets.

This module provides operations on finite sets and relations represented as
sets. Intuitively, a set is a collection of elements; every element belongs to
the set, and the set contains every element.

The data representing `sofs` as used by this module is to be regarded as opaque
by other modules. In abstract terms, the representation is a composite type of
existing Erlang terms. See note on
[data types](`e:system:data_types.md#no_user_types`). Any code assuming
knowledge of the format is running on thin ice.

## Getting started

A recommended starting point for the first-time user is the examples
for the following functions:

* `relation_to_family/1`
* `restriction/2` and `drestriction/2`
* `image/2` and `inverse_image/2`
* `converse/1`

## Set theory

Given a set A and a sentence S(x), where x is a free variable, a new set B whose
elements are exactly those elements of A for which S(x) holds can be formed,
this is denoted B = \{x in A : S(x)\}. Sentences are expressed using the logical
operators "for some" (or "there exists"), "for all", "and", "or", "not". If the
existence of a set containing all the specified elements is known (as is always
the case in this module), this is denoted B = \{x : S(x)\}.

- The _unordered set_ containing the elements a, b, and c is denoted
  \{a, b, c\}. This notation is not to be confused with tuples.

  The _ordered pair_ of a and b, with first _coordinate_ a and second coordinate
  b, is denoted (a, b). An ordered pair is an _ordered set_ of two elements. In
  this module, ordered sets can contain one, two, or more elements, and
  parentheses are used to enclose the elements.

  Unordered sets and ordered sets are orthogonal, again in this module; there is
  no unordered set equal to any ordered set.

- The _empty set_ contains no elements.

  Set A is _equal_{: #equal } to set B if they contain the same elements, which
  is denoted A = B. Two ordered sets are equal if they contain the same number
  of elements and have equal elements at each coordinate.

  Set B is a _subset_{: #subset } of set A if A contains all elements that B
  contains.

  The _union_{: #union } of two sets A and B is the smallest set that contains
  all elements of A and all elements of B.

  The _intersection_{: #intersection } of two sets A and B is the set that
  contains all elements of A that belong to B.

  Two sets are _disjoint_{: #disjoint } if their intersection is the empty set.

  The _difference_{: #difference } of two sets A and B is the set that contains
  all elements of A that do not belong to B.

  The _symmetric difference_{: #symmetric_difference } of two sets is the set
  that contains those element that belong to either of the two sets, but not
  both.

  The _union_{: #union_n } of a collection of sets is the smallest set that
  contains all the elements that belong to at least one set of the collection.

  The _intersection_{: #intersection_n } of a non-empty collection of sets is
  the set that contains all elements that belong to every set of the collection.

- The _Cartesian product_{: #Cartesian_product } of two sets X and Y, denoted
  X × Y, is the set \{a : a = (x, y) for some x in X and for some y in Y\}.

  A _relation_{: #relation } is a subset of X × Y. Let R be a relation. The fact
  that (x, y) belongs to R is written as x R y. As relations are sets, the
  definitions of the last item (subset, union, and so on) apply to relations as
  well.

  The _domain_{: #domain } of R is the set \{x : x R y for some y in Y\}.

  The _range_{: #range } of R is the set \{y : x R y for some x in X\}.

  The _converse_{: #converse } of R is the set \{a : a = (y, x) for some
  (x, y) in R\}.

  If A is a subset of X, the _image_{: #image } of A under R is the set \{y :
  x R y for some x in A\}. If B is a subset of Y, the _inverse image_{:
  #inverse_image } of B is the set \{x : x R y for some y in B\}.

  If R is a relation from X to Y, and S is a relation from Y to Z, the _relative
  product_{: #relative_product } of R and S is the relation T from X to Z
  defined so that x T z if and only if there exists an element y in Y such that
  x R y and y S z.

  The _restriction_{: #restriction } of R to A is the set S defined so that
  x S y if and only if there exists an element x in A such that x R y.

  If S is a restriction of R to A, then R is an _extension_{: #extension } of S
  to X.

  If X = Y, then R is called a relation _in_ X.

  The _field_{: #field } of a relation R in X is the union of the domain of R
  and the range of R.

  If R is a relation in X, and if S is defined so that x S y if x R y and not
  x = y, then S is the _strict_{: #strict_relation } relation corresponding to
  R. Conversely, if S is a relation in X, and if R is defined so that x R y if
  x S y or x = y, then R is the _weak_{: #weak_relation } relation corresponding
  to S.

  A relation R in X is _reflexive_ if x R x for every element x of X, it is
  _symmetric_ if x R y implies that y R x, and it is _transitive_ if x R y and
  y R z imply that x R z.

- A _function_{: #function } F is a relation, a subset of X × Y, such that the
  domain of F is equal to X and such that for every x in X there is a unique
  element y in Y with (x, y) in F. The latter condition can be formulated as
  follows: if x F y and x F z, then y = z. In this module, it is not required
  that the domain of F is equal to X for a relation to be considered a function.

  Instead of writing (x, y) in F or x F y, we write F(x) = y when F is a
  function, and say that F maps x onto y, or that the value of F at x is y.

  As functions are relations, the definitions of the last item (domain, range,
  and so on) apply to functions as well.

  If the converse of a function F is a function F', then F' is called the
  _inverse_{: #inverse } of F.

  The relative product of two functions F1 and F2 is called the _composite_{:
  #composite } of F1 and F2 if the range of F1 is a subset of the domain of F2.

- Sometimes, when the range of a function is more important than the function
  itself, the function is called a _family_.

  The domain of a family is called the _index set_, and the range is called the
  _indexed set_.

  If x is a family from I to X, then x\[i] denotes the value of the function at
  index i. The notation "a family in X" is used for such a family.

  When the indexed set is a set of subsets of a set X, we call x a _family of
  subsets_{: #family } of X.

  If x is a family of subsets of X, the union of the range of x is called the
  _union of the family_ x.

  If x is non-empty (the index set is non-empty), the _intersection of the
  family_ x is the intersection of the range of x.

  In this module, the only families that are considered are families of subsets
  of some set X; in the following, the word "family" is used for such families
  of subsets.

- A _partition_{: #partition } of a set X is a collection S of non-empty subsets
  of X whose union is X and whose elements are pairwise disjoint.

  A relation in a set is an _equivalence relation_ if it is reflexive,
  symmetric, and transitive.

  If R is an equivalence relation in X, and x is an element of X, the
  _equivalence class_{: #equivalence_class } of x with respect to R is the set
  of all those elements y of X for which x R y holds. The equivalence classes
  constitute a partitioning of X. Conversely, if C is a partition of X, the
  relation that holds for any two elements of X if they belong to the same
  equivalence class, is an equivalence relation induced by the partition C.

  If R is an equivalence relation in X, the _canonical map_{: #canonical_map }
  is the function that maps every element of X onto its equivalence class.

- [](){: #binary_relation } Relations as defined above (as sets of ordered
  pairs) are from now on referred to as _binary relations_.

  We call a set of ordered sets (x\[1], ..., x\[n]) an _(n-ary) relation_{:
  #n_ary_relation }, and say that the relation is a subset of the [](){:
  #Cartesian_product_tuple } Cartesian product X\[1] × ... × X\[n], where x\[i]
  is an element of X\[i], 1 <= i <= n.

  The _projection_{: #projection } of an n-ary relation R onto coordinate i is
  the set \{x\[i] : (x\[1], ..., x\[i], ..., x\[n]) in R for some
  x\[j] in X\[j], 1 <= j <= n and not i = j\}. The projections of a binary
  relation R onto the first and second coordinates are the domain and the range
  of R, respectively.

  The relative product of binary relations can be generalized to n-ary relations
  as follows. Let TR be an ordered set (R\[1], ..., R\[n]) of binary relations
  from X to Y\[i] and S a binary relation from (Y\[1] × ... × Y\[n]) to Z. The
  _relative product_{: #tuple_relative_product } of TR and S is the binary
  relation T from X to Z defined so that x T z if and only if there exists an
  element y\[i] in Y\[i] for each 1 <= i <= n such that x R\[i] y\[i] and
  (y\[1], ..., y\[n]) S z. Now let TR be a an ordered set (R\[1], ..., R\[n]) of
  binary relations from X\[i] to Y\[i] and S a subset of X\[1] × ... × X\[n].

  The _multiple relative product_{: #multiple_relative_product } of TR and S is
  defined to be the set \{z : z = ((x\[1], ..., x\[n]), (y\[1],...,y\[n])) for
  some (x\[1], ..., x\[n]) in S and for some (x\[i], y\[i]) in R\[i],
  1 <= i <= n\}.

  The _natural join_{: #natural_join } of an n-ary relation R and an m-ary
  relation S on coordinate i and j is defined to be the set \{z : z =
  (x\[1], ..., x\[n],  y\[1], ..., y\[j-1], y\[j+1], ..., y\[m]) for some
  (x\[1], ..., x\[n]) in R and for some (y\[1], ..., y\[m]) in S such that
  x\[i] = y\[j]\}.

## Sets handled by this module

The sets recognized by this module are represented
by elements of the relation Sets, which is defined as the smallest set such
that:

- For every atom T, except '\_', and for every term X, (T, X) belongs to Sets
  (_atomic sets_).
- (\['\_'], []) belongs to Sets (the _untyped empty set_).
- For every tuple T = \{T\[1], ..., T\[n]\} and for every tuple X =
  \{X\[1], ..., X\[n]\}, if (T\[i], X\[i]) belongs to Sets for every
  1 <= i <= n, then (T, X) belongs to Sets (_ordered sets_).
- For every term T, if X is the empty list or a non-empty sorted list
  \[X[1], ..., X\[n]] without duplicates such that (T, X\[i]) belongs to Sets
  for every 1 <= i <= n, then (\[T], X) belongs to Sets (_typed unordered
  sets_).

An _external set_{: #external_set } is an element of the range of Sets.

A _type_{: #type } is an element of the domain of Sets.

If S is an element (T, X) of Sets, then T is a _valid type_{: #valid_type } of
X, T is the type of S, and X is the external set of S. `from_term/2` creates a
set from a type and an Erlang term turned into an external set.

The sets represented by Sets are the elements of the range of function Set
from Sets to Erlang terms and sets of Erlang terms:

- Set(T,Term) = Term, where T is an atom
- Set(\{T\[1], ..., T\[n]\}, \{X\[1], ...,  X\[n]\}) =
  (Set(T\[1], X\[1]), ...,  Set(T\[n], X\[n]))
- Set(\[T], \[X[1], ..., X\[n]]) = \{Set(T, X\[1]), ..., Set(T, X\[n])\}
- Set(\[T], []) = \{\}

When there is no risk of confusion, elements of Sets are identified with the
sets they represent. For example, if U is the result of calling `union/2` with
S1 and S2 as arguments, then U is said to be the union of S1 and S2. A more
precise formulation is that Set(U) is the union of Set(S1) and Set(S2).

The types are used to implement the various conditions that sets must fulfill.
As an example, consider the relative product of two sets R and S, and recall
that the relative product of R and S is defined if R is a binary relation to Y
and S is a binary relation from Y. The function that implements the relative
product, `relative_product/2`, checks that the arguments represent binary
relations by matching \[\{A,B\}] against the type of the first argument (Arg1
say), and \[\{C,D\}] against the type of the second argument (Arg2 say). The
fact that \[\{A,B\}] matches the type of Arg1 is to be interpreted as Arg1
representing a binary relation from X to Y, where X is defined as all sets
Set(x) for some element x in Sets the type of which is A, and similarly for Y.
In the same way Arg2 is interpreted as representing a binary relation from W to
Z. Finally it is checked that B matches C, which is sufficient to ensure that W
is equal to Y. The untyped empty set is handled separately: its type, \['\_'],
matches the type of any unordered set.

A few functions of this module (`drestriction/3`, `family_projection/2`,
`partition/2`, `partition_family/2`, `projection/2`, `restriction/3`,
`substitution/2`) accept an Erlang function as a means to modify each element of
a given unordered set. [](){: #set_fun } Such a function, called SetFun in the
following, can be specified as a functional object (fun), a tuple
`{external, Fun}`, or an integer:

- If SetFun is specified as a fun, the fun is applied to each element of the
  given set and the return value is assumed to be a set.
- If SetFun is specified as a tuple `{external, Fun}`, Fun is applied to the
  external set of each element of the given set and the return value is assumed
  to be an external set. Selecting the elements of an unordered set as external
  sets and assembling a new unordered set from a list of external sets is in the
  present implementation more efficient than modifying each element as a set.
  However, this optimization can only be used when the elements of the unordered
  set are atomic or ordered sets. It must also be the case that the type of the
  elements matches some clause of Fun (the type of the created set is the result
  of applying Fun to the type of the given set), and that Fun does nothing but
  selecting, duplicating, or rearranging parts of the elements.
- Specifying a SetFun as an integer I is equivalent to specifying
  `{external, fun(X) -> element(I, X) end}`, but is to be preferred, as it makes
  it possible to handle this case even more efficiently.

Examples of valid SetFuns:

```erlang
fun sofs:union/1
fun(S) -> sofs:partition(1, S) end
fun(S) -> sofs:from_term(sofs:no_elements(S)) end
{external, fun(A) -> A end}
{external, fun({A,_,C}) -> {C,A} end}
{external, fun({_,{_,C}}) -> C end}
{external, fun({_,{_,{_,E}=C}}) -> {E,{E,C}} end}
2
```

Examples of invalid SetFuns:

```erlang
fun sofs:no_elements/1
{external, fun(A) -> 2 * A end}
{external, fun({A,B,C}) -> A + B + C end}
{external, fun lists:sum/1}
```

The order in which a SetFun is applied to the elements of an unordered set is
not specified, and can change in future versions of this module.

The execution time of the functions of this module is dominated by the time it
takes to sort lists. When no sorting is needed, the execution time is in the
worst case proportional to the sum of the sizes of the input arguments and the
returned value. A few functions execute in constant time: `from_external/2`,
`is_empty_set/1`, `is_set/1`, `is_sofs_set/1`, `to_external/1` `type/1`.

The functions of this module exit the process with a `badarg`, `bad_function`,
or `type_mismatch` message when given badly formed arguments or sets the types
of which are not compatible.

When comparing external sets, operator `==/2` is used.

## See Also

`m:digraph`, `m:gb_sets`, `m:gb_trees`, `m:maps`, `m:orddict`, `m:ordsets`, `m:sets`
