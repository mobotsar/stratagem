# stratagem

λ-calculus interpretation with support for user-defined distfix syntax!

Stratagem is an educational tool for learning the simple λ-calculus quickly and permanently. Students should use the tool to develop their own small languages by encoding relevant constructs and binding them to syntax that they design. Given such a set of bindings, the tool will generate a parser and provide an interpreter that can be used to execute programs written in the resulting language.

This enables students to "learn by doing", helping them to commit to memory the basic cannonical encodings within λ-calculus and the thought processes involved in developing them. After using this tool, they will have gained experience working with formal systems and an inutition about λ-calculus in particular.

### Use

Use of stratagem happens in two parts.

First, one must write a (possibly empty) distfix precedence grammar. This is accomplished by populating a `.stgm` file with a series of syntax extensions and associated rewrite rules. `.stgm` syntax is described in the [Syntax](#Syntax) section. Given a file `foo.stgm`, pass it to inpterpreter generator by invoking `stratagem --new foo.stmg`. An executable file, `foo.exe` will be produced. This is the interperter for the language defined in `foo.stgm`.

Second, one simply uses `foo.exe` as they would an interpreter for any other language. Write code that conforms to the language described, and execute it by passing the file as an argument to a `foo.exe` invocation. Given a source file `foo_src.txt`, `foo.exe foo_src` will execute the code.

### Syntax

`stgm` syntax mimics Agda's mixfix operator syntax. The following defines booleans and boolean operations.

```agda
tci 0 true = λx.λy.x
tci 0 false = λx.λy.y
inl 1 _or_ = ((λp.(λq.(p p) q) \1) \2)
inl 2 _and_ = ((λp.(λq.(p q) p) \1) \2)
pre 3 not_ = (λp.(λa.(λb.(p b)) a) \1)
```

The first column denotes the operator class. The possible classes are `tci` (closed/atomic), `pre` (prefix), `aft` (postfix), `inl` (left-associative infix), and `inr` (right-associative infix). The second column denotes the precedence of the operator, where operators with higher numbers bind tighter than those with lower numbers. Finally, we have the operators themselves. Each underscore indicates an expression hole, i.e. a parameter of the operator. These holes are numbered, starting at one, from left to right, and can be referenced in the corresponding expression to the right of the equals by prefacing the number with a backslash.
