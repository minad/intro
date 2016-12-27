# My current Haskell prelude

Design decisions:

* Everything should be in one file (Unfortunately we need Intro.Trustworthy for Safe Haskell)
* Conservative extension over the base Prelude
* Rely only on very common external libraries
* Export everything explicitly
* Export only total functions or provide safe alternatives (Very few exceptions like div etc.)
* Prefer Text over String, provide ConvertibleStrings
* Provide monad transformer facilities
* Provide container types
* Prefer generic functions
* Debugging facilities are provided with warnings
* Don't provide error, only panic instead
* Avoid writing custom functions, only export established stuff
* Compatibility with Control.Lens
