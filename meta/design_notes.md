### Overview

First thing's first -- take a precedence grammar, give it to aasam, get a cfg, give it to a parser generator, get a parser, call it the parser.
Take a source file, give it to the parser, get an ast, rewrite it, get a simple lambda tree, give it to a lambda calculus interpreter, get a result, display it.

It's probably worth reading the documention of `Aasam.m` [here](https://hackage.haskell.org/package/aasam-0.2.0.0/docs/Aasam.html), to ensure understanding of some references made in this document.

### Some basic principles

Operators may appear in the rhs of other operators, but dependency may not be cyclic, neither may operators appear in their own rhs.
Positary operators are defined as shown in the readme, but rules for atoms (nullary operators) are given by a user-provided generation function :: X -> Y, where X is the type of valid atom lhs and Y is the type of mix-strings (lambda strings containing zero or more unexpanded userdef'd operators). This function produces "exverse" (a.k.a. rewrite from userdef'd lang to pure lambda) and "inverse" (from pure to udef) rule(s) based on the finite set of actual atoms present in the source at hand. Critically, this happens post-parser-generation, as the parser deals with unexpanded mix-strings, i.e. valid programs in the user language. (Call the language/syntax defined by the user "L".)

(Consider implementing multiple reduction semantics, especially the addition of laziness.)

For convenience reasons, we probably want to allow violations of m's continuity requirement (the fifth error variety described in the `m` documentation) in the specification of operators. This requires a transformation of the given precedence grammar to render the precedences continuous before it's passed to m. That's not hard, just something to keep in mind.

Custom operators can't make use of any character in the pure lambda operator syntax. So, no ".", and no "(" or ")". The pure syntax is given below.
```
T ::= V
    | ( V . T )
    | ( T T )
```
where T is the nonterminal of terms and V is the pseudo-terminal of identifiers.

### Grafting the pure syntax to the udef'd syntax

A closed production with the atom "PURE" must be added to the precedence grammar before `m` runs. This will result in a CE cfg production going to the terminal "PURE". This production will be replaced with CE -> T.

In the future, examine part VI of Annika Aasa's PhD thesis for some insight into how to make this work with an arbitrary pure syntax, i.e. one where not all productions are closed. Use of a more standard syntax, like the one described in VT's opendsa, may be desireable.

### Whitespace

~~This is separate but related. It's possible that some operator (lambda application, juxtaposition, for example) may be most naturally defined with whitespace as a terminal. However, the L interpreter should be whitespace insensitive whenever possible, for convenience reasons. Consider solutions that satisfy both requirements (a flag and mark approach seems reasonable).~~

~~I intend to lex the source before passing it to `GLL`, so this sensitive sensitivity could probably be handled in the lexer. The trouble with these dual goals is that most general aproaches become to difficult to reason about with respect to ambiguity in the parsing machinery. One possible solution is to only allow whitespace terminals a intermediate terminals of closed productions. In principle, it would be possible to allow them between other terminals in operators of any class, since the basis of the distfix extension of `m` is that any closed symbol sequence can be treated like an atomic expression (AE from the paper) when it comes to questions of ambiguity (whether I actually do that depeneds on how much of a pain in the ass it shapes up to be). I can't just do a normal stack-based thing where I track what operator context is active at each token, because operators need not have unique terminators, so this could take some thought. It might not be possible.~~

~~Alternatively, I could modify the either the input or the output of `m` to be whitespace insensitive where appropriate. This definitely _is_ possible, but could potentially be a little more work (especially in the testing department). All the same, because this is the approach that I favor at the moment, since I know I can do it. Modification of the precedence grammar is probably easier than futzing about with a generated cfg. You still can't define the standary lambda syntax as a distfix grammar, but this gets you closer.~~

Upon further consideration, this whole "sensitive sensitivy" thing was a truly terrible idea, and I can't be arsed to make it work anyway.

### A quick note

Don't try to do some weird shit where users can specialize their operators' holes to terms in specific forms. That's very complicated for essentially zero gain, as the basic calculus already provides all they need (which is the whole point). New binding syntax easily wraps the existing binding construct.

### Display of atoms in final results of evaluation

First, it's important that this behavior can be disabled easily, to facilitate quick cross-checking by students of the pure and L-encoded (when flattened, L-encoded trees are mix-strings) representations of their program's result.

One possible solution, probably the most promising, is to wait until provided with a source to produce a finite set of concrete, actionable rewrite rules. ~~Rules are generated in pairs where each item is the inverse of the other, and~~ There is a bijection between rul~~e pair~~s and the set of unique atoms in the source. That is, for each unique atom, 'A', apply the generation function to 'A'. It yields a ~~pair of~~ rule~~s~~ that uniquely corresponds to 'A'. (This works because of the initial sequence restrictions on arguments to `m`). By retaining inverse information from the rule generation function in the ultimately resultant L interpreter, we can display atomic expressions in a result in either pure or L-encoded form. Incidentally, this serves to simplify later stages of the interpreter that make use of exverse rules as well, as they can be generic over operator arity (and by extension, over the class of the precedence production wherein that operator is defined). Since lambda terms may appear in results that correspond to atoms not actually present in the source, we probably need to, instead of generating rules in pairs, find the inverse function of the atom generation function, and use an analogous technique to before to generate inverse rewrite rules based on the actual result after the program finishes executing.

The valid forms for atoms to take, both as a practical matter of implementation as well as to reduce incidence of user error in the definition, should be somewhat more restricted than if I were to allow arbitrary regex. One easy solution would be to allow atoms of any string not containing a character found in non-atomic syntax. This has the potential downside of potentially causing a preexisting atom to become invalid upon the addition of a positary operator with a common character. A possible solution is to reserve a certain set of characters for use in atoms (and disallow all others), but this may be prohibitively restrictive. Currently, I lean toward the first option, though whitespace and control sequences should probably not be allowed in atoms. So, atoms are specified by a regex, which is checked against the character restrictions, and if valid, used to produce the set of all unique atoms in the source (call this set AE). Alternatively, the language of possible atoms could be deduced from the rest of the syntax and the given character restrictions, and atomatically considered by the atomic rule function. That is, each string of allowed atom characters in the source would be counted as an atom. For each item in AE, the function would return `Just (...)` if the item had a valid mapping, else `Nothing`.

### Initial interpretation stages

Thus, the process begins as follows.
A (possibly non-continuous) precedence grammar is assembled from the lhs of the positary operators. Each unique atom is collected and used to generate a Closed PrecedenceProduction which is then added to the grammar. The grammar is then transformed to be continuous (if possible without altering semantics -- if not, halt) and passed to `Aasam.m`, producing a suitable context-free grammar. This grammar is then passed to the appropriate `GLL` function (after being mapped to the required analogous grammar datatype), creating a parser for L. The atom correspondence function is passed AE, producing a set of rewrite pairs in the form (exverse, inverse). The positary rule pairs are also constructed and added to the set, which is then passed, alongside the parser -- at least conceptually -- to the next stage.

### Utilizing the products of above

After we generate rewrite rules and parse the source, gll returns a parse tree. An function, R, generic over the contents of that tree, will apply the exverse rules, producing a lambda-term AST. The AST will be passed to the appropriate lambda-calculus interpreter (probably selected at L spec time by the user, from various options of differing semantics), which will then return a lambda term in normal form, (unless the input diverges, but that's actually someone else's problem). If requested at spec time, another function, S, applies the inverse rules to the normal tree. The tree is then `Show`n to the user (probably also parametrically with respect to a user-provided option).

### Physical assemblage of the L interpreter

As it turns out, this is kinda tricky to do with a measure of elegance. What I really _want_ is to do this in common lisp, or anything homoiconic really, but what I'm probably _going_ to do is use haskell. Anyway, let's explore what the haskell approach would look like, so I can decide whether it's worth using CL and introducing an FFI shim just to avoid some ugliness. First, I need to think about what the L interpreter actually acts like. Is it a standalone executable, fire and forget type deal, or is it a REPL? The REPL is better for my purposes, almost certainly, so that solves that question I guess. In that case, homoiconicity is far less important, as I can use `GLL.Parser.parse` partially applied (probably with -ffull-laziness for performance, though that's worth benchmarking) to evaluate source as it comes in. Because the runtime persists between most evaluations, this is basically fine. Perhaps I provide an option to execute an L program from the CLI, in which case we still incur the parser generation penalty per-parse, but that means stratagem can have no dependency on GHC and never hits the disk, so it's worth it. Additionally, it seems possible that the generation penalty is rather less than the work of actually parsing (that's something to investigate at some point, but conversation in the haskell discord seems to suggest it is). So, in essence, there is no physical assemblage of any L interpreter. Everything, from stgm parsing to L-source parsing happens with a single invocation of `stratagem`.

### UI of the L interpreter

First thing, since there's going to be a CLI, we probably want some sort of config file so the user doesn't have to provide the same option over and over and over and over and over and over and over again. Probably how that will work is that stratagem checks for a config file in the root of the current directory (config files in form *.rc, maybe). That facilitates a sort of "project directory" thing for each separate L, which is kinda neat. (Take care that this doesn't accidentally turn into a build system. (Oh shit oh god it's turned into a build system!))

Now for the actual thing. When `stratagem` is invoked, it'll read the stgmrc and start a repl (unless it's been passed the --oneshot flag, in which case it _won't_), then it will execute the source iff source was provided. The stgmrc is literally just a line-by-line list of options that are passed to `stratagem` exactly as though they were given on the command line in the same order. For example, `stratagem --oneshot --reduction name EpicSourceFile` with no rc is precisely the same thing as `stratagem` with the following rc.

```
--oneshot
--reduction name
EpicSourceFile
```
The full list of options and their functions follows.

- `x` where x is just the path to some L source to interpret.
- `--oneshot` does the thingy. you know. Depends on `source`. Excludes `--repl`.
- `--repl` the opposite of `--oneshot`. Excludes `--oneshot`.
- `--reduction x` where x is some valid reduction strategy. I think there are 4 I want to support, call-by-value, call-by-name, normal order, and applicative order. Options for those being "value", "name", "normal", and "applicative".
- `--stgm x` where x is the path to a stgm file.
- `--rc x` where x is the path to an rc file. Overrides any found during the in-directory search.
- `--create x` creates a new directory, x, and populates it with x.stgm and x.rc files, as well as a source file, containing a pure lambda hello world, called hello.x, where x is whatever you want to call your language. Excludes all other options
