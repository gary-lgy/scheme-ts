Open-source implementations of the programming language *Source*. Source
is a series of small subsets of JavaScript, designed for teaching
university-level programming courses for computer science majors,
following Structure and Interpretation of Computer Programs, JavaScript
Adaptation (<https://sicp.comp.nus.edu.sg>).

Usage
=====

To build,

``` {.}
$ git clone https://<url>/x-slang.git
$ cd x-slang
$ yarn
$ yarn build
```

To add \"x-slang\" to your PATH, build it as per the above
instructions, then run

``` {.}
$ cd dist
$ npm link
```

If you do not wish to add \"x-slang\" to your PATH, replace
\"x-slang\" with \"node dist/repl/repl.js\" in the following examples.

To try out *Source* in a REPL, run

``` {.}
$ x-slang '1 * 1'
```

Hint: In `bash` you can take the `PROGRAM_STRING` out
of a file as follows:

``` {.}
$ x-slang "$(< my_source_program.js)"
```

Documentation
=============

Source is documented here: <https://sicp.comp.nus.edu.sg/source/>

Testing
=======
`x-slang` comes with an extensive test suite. To run the tests after you made your modifications, run 
`yarn test`. Regression tests are run automatically when you want to push changes to this repository. 
The regression tests are generated using `jest` and stored as snapshots in `src/\_\_tests\_\_`.  After modifying `x-slang`, carefully inspect any failing regression tests reported in red in the command line. If you are convinced that the regression tests and not your changes are at fault, you can update the regression tests as follows:  
``` {.}
$ yarn test -- --updateSnapshot
```

Error messages
==============

To enable verbose messages, have the statement `"enable verbose";` as the first line of your program.

There are two main kinds of error messages: those that occur at runtime
and those that occur at parse time. The first can be found in
`interpreter-errors.ts`, while the second can be found in `rules/`.

Each error subclass will have `explain()` and `elaborate()`. Displaying the
error will always cause the first to be called; the second is only
called when verbose mode is enabled. As such, `explain()` should be made
to return a string containing the most basic information about what the
error entails. Any additional details about the error message, including
specifics and correction guides, should be left to `elaborate()`.

Please remember to write test cases to reflect your added
functionalities. The god of this repository is self-professed to be very
particular about test cases.

Using your x-slang in local Source Academy
===========================================

A common issue when developing modifications to x-slang is how to test
it using your own local frontend. Assume that you have built your own
x-frontend locally, here is how you can make it use your own
x-slang, instead of the one that the Source Academy team has deployed
to npm.

First, build and link your local x-slang:
``` {.}
$ cd x-slang
$ yarn build
$ yarn link
```
Then, from your local copy of x-frontend:
``` {.}
$ cd x-frontend
$ yarn link "x-slang"
```

Then start the frontend and the new x-slang will be used. 
