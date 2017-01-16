# Intro: My current Haskell Prelude

[![Hackage](https://img.shields.io/hackage/v/intro.svg)](https://hackage.haskell.org/package/intro)
[![Build Status](https://secure.travis-ci.org/minad/intro.png?branch=master)](http://travis-ci.org/minad/intro)

Intro is a modern Prelude which provides safe alternatives
for most of the partial functions and follows other
best practices, e.g., Text is preferred over String.
For String overloading the extension 'OverloadedStrings' should be used.
Container types and Monad transformers are provided.

Most important - this Prelude tries to keep things simple.
This means it just reexports from base and commonly used libraries
and doesn't invent its own stuff.
Furthermore the Prelude is not scattered over many files.
Everything is exported explicitly to improve the quality of the documentation.

List of design decisions:

* Simplicity: Everything is in one file (Unfortunately we need Intro.Trustworthy for Safe Haskell)
* Conservative extension over the base Prelude
* Rely only on very common external libraries
* Avoid writing custom functions
* Export everything explicitly for good documentation
* Export only total functions or provide safe alternatives (Very few exceptions like div etc.)
* Prefer Text over String, provide ConvertibleStrings
* Provide Monad transformers
* Provide container types
* Prefer generic functions
* Debugging functions, like 'Intro.Trustworthy.trace' and 'undefined' are available but produce compile time warnings
* Don't provide error, only panic instead
* Compatibility with Control.Lens
