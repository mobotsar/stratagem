# stratagem

λ-calculus interpretation with support for user-defined distfix syntax!

Stratagem is an educational tool for learning the simple λ-calculus quickly and permanently. Students should use the tool to develop their own small languages by encoding relevant constructs and binding them to syntax that they design. Given such a set of bindings, the tool will generate a parser and provide an interpreter that can be used to execute programs written in the resulting language.

This enables students to "learn by doing", helping them to commit to memory the basic cannonical encodings within λ-calculus and the thought processes involved in developing them. After using this tool, they will have gained experience working with formal systems and an inutition about λ-calculus in particular.

## Use

Use of stratagem happens in two parts. First, one must write a (possibly empty) distfix precedence grammar. This is accomplished by populating a `.stgm` file with a series of syntax extensions and associated rewrite rules. `.stgm` syntax is described in the [Syntax](#Syntax) section.
