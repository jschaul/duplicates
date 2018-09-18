# duplicates

Find duplicate sentences or sentence fragments in a large (e.g. book-length) text file.

Behaviour is primitive; text is only split on new lines and punctuation, and any splits shorter than 20 characters are ignored. While nothing fancy is done (i.e. better-performing suffix trees are not used, I'm using simple lists), performance for a 400 page test document is sub-second.

**Status: experimental**. Things are hardcoded.

## Requirements

* [haskell stack](https://docs.haskellstack.org/en/stable/README/) to compile this project.
* Since few people write books in text or markdown, you probably want [pandoc](http://pandoc.org/)

## How to use

create a `input.txt` file in the current directory, e.g. using pandoc to convert if necessary:

```
pandoc <input file> -o input.txt --wrap=none
```

Compile with `make build` and analyse for duplicates with `make run`


