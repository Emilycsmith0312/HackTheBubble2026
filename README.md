# HackTheBubble2026
Our submission for STACS Hack the Bubble 2026.

# Clip Clop

Clip Clop is a functional language about horses. Every variable is a horse.
Horses have moods. You will not enjoy this.

```
cabal run bubble2026     # start the REPL
cabal test               # run the tests
```

## Contents

- [A quick ride](#a-quick-ride)
- [Comments](#comments)
- [Values and types](#values-and-types)
- [Horses (variables)](#horses-variables)
- [Operators](#operators)
- [Keyword operators](#keyword-operators)
- [Functions](#functions)
- [Whinny (case expressions)](#whinny-case-expressions)
- [Electrocution](#electrocution)
- [Moods](#moods)
- [Sound effects](#sound-effects)
- [The REPL](#the-repl)
- [Reserved words](#reserved-words)

## A quick ride

```
pssssst factorial, the hard way
the SingularHorse fact ridden by SingularHorse n says \
  whinny n \
    snort cl0p says cl1p stomp \
    nicker says n * giddyup fact with n - cl1p

giddyup fact with cl1pcl0pcl1p
```

prints `cl1pcl1pcl1pcl1pcl0pcl0pcl0p` (120).

## Comments

`pssssst` starts a comment that runs to the end of the line. It must be spelled
with exactly five `s`s.

```
pssssst nobody can hear this
the SingularHorse a says cl1p   pssssst a is one
```

A `pssssst` inside a string or character literal, or on the end of a longer
word (`xpssssst`), is not a comment.

## Values and types

| Type            | Meaning  | Literals                                  |
|-----------------|----------|-------------------------------------------|
| `SingularHorse` | integer  | `cl1pcl0pcl1p`, `-cl1pcl0p`               |
| `PairOfHorses`  | float    | `cl1pcl0p.cl1p`, `-cl0p.cl1p`             |
| `Shetland`      | character| `'a'`                                     |
| `SuffolkPunch`  | string   | `"hello"`                                 |
| `Appaloosa`     | boolean  | `neighneigh` (true), `neigh` (false)      |
| `Unitcorn`      | unit     | `()`                                      |
| `Steed`         | function | see [Functions](#functions)               |

### Numbers

Numbers are written, and printed, in binary. Each digit is a word: `cl1p` is 1
and `cl0p` is 0. The digits are written together with no spaces.

| Clip Clop          | Decimal |
|--------------------|---------|
| `cl0p`             | 0       |
| `cl1p`             | 1       |
| `cl1pcl0p`         | 2       |
| `cl1pcl0pcl1p`     | 5       |
| `-cl1pcl0p`        | -2      |
| `cl1pcl0p.cl1p`    | 2.5     |
| `cl0p.cl0pcl1p`    | 0.25    |

A number with a `.` is a `PairOfHorses`; without one it is a `SingularHorse`.

Strings cannot contain `"` and characters cannot be `'`; there are no escapes.

## Horses (variables)

```
the TYPE NAME says EXPRESSION
```

```
the SingularHorse a says cl1pcl0p
the SuffolkPunch greeting says "hello " ++ "horse"
the Appaloosa happy says neighneigh
```

- Names start with a lowercase letter, followed by letters and digits.
- The expression must have the declared type, or the horse is not created.
- Saying the same thing again with a new value replaces the old one.

An expression on its own is printed. `saysomethinggirl` in front does the same
thing, more politely:

```
a + cl1p
saysomethinggirl a + cl1p
```

## Operators

| Operator | Meaning          | Works on                                        |
|----------|------------------|-------------------------------------------------|
| `+`      | add              | numbers                                         |
| `-`      | subtract         | numbers                                         |
| `*`      | multiply         | numbers                                         |
| `/`      | divide           | numbers; two integers give a float unless exact |
| `%`      | modulus          | integers                                        |
| `^`      | power            | number to an integer power                      |
| `++`     | join             | strings                                         |
| `&&`     | and              | booleans                                        |
| `!&`     | nand             | booleans                                        |
| `\|\|`   | or               | booleans                                        |
| `!\|`    | nor              | booleans                                        |
| `x\|`    | xor              | booleans                                        |
| `->`     | implies          | booleans                                        |
| `==`     | equals           | two numbers of the same type, strings, booleans |
| `!`      | not (prefix)     | booleans                                        |

**There is no precedence.** Every binary operator is evaluated strictly left to
right, so `cl1p + cl1p * cl1pcl0p` is `(1 + 1) * 2 = 4`. Use brackets.

## Keyword operators

Each of these goes in front of an expression and applies to **everything after
it**: `geld a + b` is `geld (a + b)`. To stop it early, put brackets around the
whole thing: `(geld a) + b`. They can only start an expression or a bracket,
not follow an operator: write `a + (geld b)`, not `a + geld b`.

| Keyword           | Meaning                                                              |
|-------------------|----------------------------------------------------------------------|
| `geld`            | absolute value                                                       |
| `toSuffolkPunch`  | convert to a string                                                  |
| `amalgamare`      | convert to an integer (strings must hold a `cl1p`/`cl0p` number)     |
| `maretosis`       | convert to a float                                                   |
| `addSpots`        | convert to a boolean (zero is `neigh`, anything else `neighneigh`)   |
| `speakupgirl`     | upper case a string (numbers become `CL1PCL0P`)                      |
| `quietdowngirl`   | lower case a string (numbers are whispered in decimal)               |
| `electrocute`     | see [Electrocution](#electrocution)                                  |
| `!`               | boolean not                                                          |

## Functions

A function is a horse that says nothing until it is **ridden**. Its parameters
are its riders.

```
the RETURNTYPE NAME ridden by TYPE RIDER and TYPE RIDER says EXPRESSION
```

```
the SingularHorse double ridden by SingularHorse x says x + x
the SingularHorse add ridden by SingularHorse x and SingularHorse y says x + y
```

Ride it with `giddyup`:

```
giddyup FUNCTION with EXPRESSION and EXPRESSION
```

```
giddyup double with a
giddyup add with a and cl1p
```

- Each argument runs as far as it can: `giddyup add with a and b + c` passes
  `b + c` as the second rider. To use the result in a bigger expression, put
  brackets round the whole call: `(giddyup add with a and b) + c`.
- The riders' types and the return type are checked when the function is
  ridden, not when it is defined.
- The body can use any horse, including itself, and including horses that are
  defined later. Riders hide horses of the same name.
- A function that rides more than 200 calls deep collapses from exhaustion.

### Functions are values

A function has the type `Steed`, whatever its riders and return type. It can be
stored, passed to another function, and returned from one.

```
the SingularHorse twice ridden by Steed f and SingularHorse x says giddyup f with giddyup f with x
giddyup twice with double and a          pssssst double (double a)

the Steed same says double               pssssst another name for double
```

### Foals (anonymous functions)

A foal is a horse too young to have a name.

```
foal RETURNTYPE ridden by TYPE RIDER and TYPE RIDER says EXPRESSION
```

```
giddyup twice with (foal SingularHorse ridden by SingularHorse n says n * n) and a
```

The body of a foal runs as far as it can, so put brackets round the foal to end
it. A foal remembers the riders of the function it was born in:

```
the Steed adder ridden by SingularHorse n says foal SingularHorse ridden by SingularHorse m says n + m
the Steed inc says giddyup adder with cl1p
giddyup inc with a                       pssssst a + 1
```

The function after `giddyup` can be a name, a bracketed expression, or another
`giddyup`:

```
giddyup giddyup adder with a with cl1p   pssssst (adder a) 1
giddyup (foal SingularHorse ridden by SingularHorse q says q) with a
```

## Whinny (case expressions)

| Keyword  | Meaning |
|----------|---------|
| `whinny` | switch  |
| `snort`  | case    |
| `nicker` | default |
| `stomp`  | break   |

```
whinny EXPRESSION
  snort EXPRESSION says EXPRESSION stomp
  snort EXPRESSION says EXPRESSION
  nicker says EXPRESSION stomp
```

```
whinny a
  snort cl1p     says "one" stomp
  snort cl1pcl0p says "two" stomp
  nicker         says "lots" stomp
```

(In the REPL a whinny must be on one line, or each line but the last must end
in `\`.)

It works like a `switch` in C, including the part everyone gets wrong:

- The whinnied value is compared with each `snort` in order. The first equal
  one is chosen. If none is, the `nicker` is chosen. If there is no `nicker`
  either, the whinny is `()`.
- The chosen body is evaluated. **Without a `stomp`, evaluation falls through**
  into the next body, and the next, until a `stomp` or the end.
- The value of the whinny is the value of the **last** body evaluated.

```
whinny cl1pcl0p
  snort cl1p     says "one"
  snort cl1pcl0p says "two"
  snort cl1pcl1p says "three" stomp
  nicker         says "lots"
```

is `"three"`: it matched `"two"`, fell into `"three"`, and stomped.

- Bodies that are skipped are never evaluated, which is what lets a recursive
  function stop. Bodies that are fallen through *are* evaluated, so their
  errors, electrocutions and moods all happen.
- Values of different types are never equal, and that is not an error.
- The `nicker` can go anywhere (falling through works from there too), but
  there can only be one.
- A whinny swallows every `snort` and `nicker` after it. To put a whinny inside
  the body of another, put brackets round the inner one.
- The arms may have different types.

## Electrocution

`electrocute` depends on what you electrocute:

| Electrocuting a… | Result |
|------------------|--------|
| number           | every printed binary digit is flipped: `electrocute cl1pcl0pcl1p` is `cl0pcl1pcl0p` |
| boolean          | it is flipped |
| string           | nothing happens to the string, but the horse is offended and **kicks you in the head**. You are concussed for the next five lines, and everything printed comes out backwards. |
| character        | the result is `()`, and if it was a horse, **the horse dies** and no longer exists. |

Electrocutions inside a function happen when the function is ridden.

## Moods

Every time a horse is used in an evaluation, there is a 1 in 8 chance that it
changes mood. Function horses have moods too. Nothing tells you when it happens.

| Mood     | What happens                                             | Cure                 |
|----------|----------------------------------------------------------|----------------------|
| Sulking  | `a is ignoring you` whenever it is used                  | `treat a`            |
| Lying    | it gives a wrong value of the right type, with no warning| `interrogate a`      |
| Ran away | `a has run away` whenever it is used                     | `honey a`            |
| Feud     | `a and b are not speaking to each other` if both are used in the same evaluation | `makeup a and b` |

- Every pair of horses used together has a 1 in 8 chance of starting a feud.
- A horse used inside a function's body counts as being used when the function
  is.
- A lying horse lies everywhere, including in `:vars`. If you store a lie in
  another horse, that horse really has the wrong value, for good.
- A lying function horse gives a function whose every result is a lie. Storing
  it under another name stores the liar.
- Lies: integers and floats are one too big, booleans are flipped, strings are
  backwards, characters are the next character.
- Moods belong to the name. Giving the horse a new value does not cure it.
- Riders are not horses and have no moods.

The cures are statements of their own:

```
treat a
interrogate a
honey a
makeup a and b
```

Using the wrong cure does nothing.

For when you need to get something done: `:seed N` restarts the moods from a
fixed seed (the same seed and the same lines give the same moods), and starting
the REPL with the environment variable `CLIPCLOP_CALM=1` turns moods off.

## Sound effects

Put sound files in the `sounds/` directory and they are played as things are
evaluated: `cl1pcl0p` plays `clip.wav` then `clop.wav`, `neigh` plays
`neigh.wav`, and so on. See [`sounds/README.md`](sounds/README.md) for the full
list of names and how to add more.

## The REPL

| Command       | Meaning                                                   |
|---------------|-----------------------------------------------------------|
| `:help`       | show help                                                 |
| `:vars`       | list the horses and what they (claim to) say              |
| `:load FILE`  | run each line of a file                                   |
| `:reset`      | release all the horses                                    |
| `:seed N`     | restart the horses' moods from seed N                     |
| `:q`, `:quit` | quit                                                      |
| `:shoo`       | say bye to every horse, then quit                         |

- Each line is one statement. End a line with `\` to continue on the next line;
  a block joined this way can hold one long statement or several statements.
  A comment may follow the `\`.
- Up/down for history, Ctrl-R to search it, Tab to complete `:` commands,
  Ctrl-C to cancel the line, Ctrl-D to quit.
- Pasting several lines runs them one after another.

## Reserved words

These cannot be used as horse names:

```
the says saysomethinggirl neigh neighneigh
geld toSuffolkPunch amalgamare maretosis addSpots speakupgirl quietdowngirl electrocute
ridden by and giddyup with foal
whinny snort nicker stomp
treat interrogate honey makeup
duplicateHorse
```

The type names start with a capital letter, so they could never be horse names
anyway.
