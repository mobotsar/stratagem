# stratagem

λ-calculus interpretation with support for user-defined distfix syntax!

Stratagem is an educational tool for learning the simple λ-calculus quickly and permanently. Students should use the tool to develop their own small languages by encoding relevant constructs and binding them to syntax that they design. Given such a set of bindings, the tool will generate an interpreter that can be used to execute programs written in the resulting language.

This enables students to "learn by doing", helping them to commit to memory the basic cannonical encodings within λ-calculus and the thought processes involved in developing them. After using this tool, they will have gained experience working with formal systems and an inutition about λ-calculus in particular.

### Use

Use of stratagem happens in two parts.

First, one must write a (possibly empty) distfix precedence grammar. This is accomplished by populating a `.stgm` file with a series of syntax extensions and associated rewrite rules. `.stgm` syntax is described in the [Syntax](#Syntax) section. Given a file `foo.stgm`, pass it to the inpterpreter generator by invoking `stratagem foo.stmg`. An executable file, `foo.exe` will be produced. This is the interperter for the language defined in `foo.stgm`.

Second, one simply uses `foo.exe` as they would an interpreter for any other language. Write code that conforms to the language described, and execute it by passing the file as an argument to a `foo.exe` invocation. Given a source file `foo_src.txt`, `foo.exe foo_src` will execute the code.

### Syntax

`stgm` syntax mimics Agda's mixfix operator syntax. The following complete `stgm` file defines booleans and boolean operations.

```php
d   true  = Lx.Ly.x
d   false = Lx.Ly.y
l 1 _or_  = ((Lp.(Lq.(p p) q) ~1) ~2)
l 2 _and_ = ((Lp.(Lq.(p q) p) ~1) ~2)
e 3 not_  = (Lp.(La.(Lb.(p b)) a) ~1)
```

The first column denotes the operator's class. The possible class indicators are `d` (closed/atomic), `e` (prefix), `t` (postfix), `l` (left-associative infix), and `r` (right-associative infix). The second column denotes the precedence of the operator, where operators with higher numbers bind tighter than those with lower numbers. Finally, we have the operators themselves. Each underscore indicates an expression hole, i.e. a parameter of the operator. These holes are numbered, starting at one, from left to right, and can be referenced in the corresponding expression to the right of the equal sign by prefacing the number with a tilde ('~'). In the event that an operator word contains a tilde, that word may be escaped with a backslash.
