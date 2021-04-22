# scheme-slang

Implementation of the programming language *Scheme* largely based on the [Revised<sup>5</sup> Report on the Algorithmic Language Scheme (R5RS)](https://schemers.org/Documents/Standards/R5RS/).

User documentation and specifications: **TODO**
Developer documentation: [DEVELOPER.md](./DEVELOPER.md)

## Usage

To build, run

```bash
yarn
yarn build
```

To try out *Scheme* in a REPL, run

```bash
node dist/repl/repl.js '(+ 1 2)'
```

Hint: In `bash` you can take the `PROGRAM_STRING` out
of a file as follows:

```bash
node dist/repl/repl.js "$(< my_scheme_program.scm)"
```

## Testing

`scheme-slang` comes with an extensive test suite. To run the tests after you made your modifications, run

```bash
yarn test
```
