# scheme-ts

Implementation of the programming language *Scheme* largely based on the [Revised<sup>5</sup> Report on the Algorithmic Language Scheme (R5RS)](https://schemers.org/Documents/Standards/R5RS/).

[Try out Scheme in the browser](https://scheme.garyliu.dev) (in a modified [Source Academy](https://github.com/source-academy/) frontend)

## Documentation

User documentation and specifications: [Documentation.pdf](./docs/Documentation.pdf)

Developer documentation: [DEVELOPER.md](./docs/DEVELOPER.md)

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

`scheme-ts` comes with an extensive test suite. To run the tests after you made your modifications, run

```bash
yarn test
```

## Credits

This repository was bootstrapped with the project template provided in CS4215 Programming Language Implementation at National University of Singapore. The template was in turn a stripped-down version of the [Source Academy](https://github.com/source-academy/)'s [`js-slang`](https://github.com/source-academy/js-slang).
