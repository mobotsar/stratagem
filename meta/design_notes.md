First thing's first -- take a precedence grammar, give it to aasam, get a cfg, give it to a parser generator, get a parser, call it the parser.
Take a source file, give it to the parser, get an ast, rewrite it, get a simple lambda tree, give it to a lambda calculus interpreter, get a result, display it.

### Some basic principles

Operators may appear in the rhs of other operators, but dependency may not be cyclic, neither may operators appear in their own rhs.
Positary operators are defined as shown in the readme, but rules for atoms (nullary operators) are given by a user-provided generation function :: X -> Y, where X is the type of valid atom lhs and Y is the type of mix-strings (lambda strings containing zero or more unexpanded userdef'd operators). This function produces exverse (a.k.a. rewrite from userdef'd lang to pure lambda) and inverse (from pure to udef) rule(s) based on the finite set of actual atoms present in the source at hand. Critically, this happens post-parser-generation, as the parser deals with unexpanded mix-strings, i.e. valid programs in the user language. (Call the language/syntax defined by the user "L".)

(Consider implementing multiple reduction semantics, especially the addition of laziness.)

For convenience reasons, we probably want to allow violations of m's continuity requirement in the specification of operators. This requires a transformation of the given precedence grammar to render the precedences continuous before it's passed to m. That's not hard, just something to keep in mind.

### Display of atoms in final results of evaluation

First, it's important that this behavior can be disabled easily, to facilitate quick cross-checking by students of the pure and L-encoded (when flattened, L-encoded trees are mix-strings) representations of their program's result.

One possible solution, probably the most promising, is to wait until provided with a source to produce a finite set of concrete, actionable rewrite rules. Rules are generated in pairs where each item is the inverse of the other, and there is a bijection between rule pairs and the set of unique atoms in the source. That is, for each unique atom, `A`, apply the generation function to 'A'. It yields a pair of rules that uniquely correspond to 'A'. (This works because of the initial sequence restrictions on arguments to `m`). By retaining inverse information from the atom correlation function in the ultimately resultant L interpreter, we can trivially display atomic expressions in a result in either pure or L-encoded form. Incidentally, this serves to simplfy later stages of the interpreter that make use of exverse rules as well, as they can be generic over operator arity (and by extension, over the class of the precedence production wherein that operator is defined).

The valid forms for atoms to take, both as a practical matter of implementation as well as to reduce incidence of user error in the definition, should be somewhat more restricted than if I were to allow arbitrary regex. One easy solution would be to allow atoms of any string not containing a character found in non-atomic syntax. This has the potential downside of potentially causing a preexisting atom to become invalid upon the addition of a positary operator with a common character. A possible solution is to reserve a certain set of characters for use in atoms (and dissallow all others), but this may be prohibitively restrictive. Currently, I lean toward the first option, though whitespace should probably not be allowed in atoms.

### Whitespace
This is sepparate but related. It's possible that some operator (lambda application, juxtaposition, for example) may be most naturally defined with whitespace as a terminal. However, the L interpreter should be whitespace insensitive whenever possible, for convenience reasons. Consider solutions that satisfy both requirements (a flag and boundary-marking approach seems reasonable).
