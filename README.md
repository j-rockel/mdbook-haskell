# mdbook-haskell

`mdbook-haskell` is an [mdBook](https://github.com/rust-lang/mdBook) preprocessor for easier Haskell code inclusion. Given a file and name, it looks for the definition of the given name in that file and inserts a Haskell codeblock containing the definition (and its associated haddock comments). 

## How to use

To build the preprocessor, clone this repository, enter it and call

```
$ stack install
```
(If you don't have stack, ghc etc. installed, you'll need to install those first - I recommend using [ghcup](https://www.haskell.org/ghcup/))

In your `book.toml`, add
```
[preprocessor.haskell]
haskell-project-root = "../"
```

To add a codeblock to a chapter's markdown file, add
```
{{#haskell-src <path_to_file> <identifier>}}
```

### Example

As an example, see the book in the `/testBook` folder: 

In `book.toml`:
```
[preprocessor.haskell]
haskell-project-root = "<insert relative path from the location of the book.toml to your haskell project's root here>"
```
In `chapter_1.md`:
```
{{#haskell-src src/Modify.hs modifyBook}}
```

## Capabilities

The preprocessor is currently built with ghc 9.2.2 and thus should be able to parse syntax allowed up to ghc 9.2.2. It uses the parsing utilities provided by `ghc-lib-parser` to parse the given source file, identify all declarations associated with the given name, combine their source locations and inserting an `{{#import <path_to_file>:<start_line>:<end_line>}}` statement inside a haskell code block, which mdbook then replaces with the respective lines of that source file.
It can retrieve the definition locations of functions, types and typeclasses but currently does not work with Template Haskell splices. If you have an idea how to fix that, I'd love to hear it!