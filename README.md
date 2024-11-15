# Ninety-nine Prolog problems (in Haskell, Prolog and maybe others)

For starters, this will include Haskell and Prolog versions
for the solutions to the original problem set
(See: https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/ ;
I can't seem to locate the original URL).

Other candidate languages:
- notation3 (Eye Reasoner)
- ErgoAI
- Some Scheme/Lisp (maybe one of my toy implementations of those?)
- for laughs: `jq`, `Perl`

## Usage Notes

### Prolog

I use SWI-Prolog:

```sh
prolog
```
>
```
Welcome to SWI-Prolog (threaded, 64 bits, version 9.0.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?-
```
then
```prolog
?- [prolog/lists].
true.

?- encode_modified([3,3,3,4,5,5,5,5], R).
R = [[3, 3], 4, [4, 5]] .

```

### Haskell

Just run the unit-tests:
```bash
stack test
# or
stack test --watch
```
