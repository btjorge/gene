# gene

Gene is a tool that aims to characterize whether a code is generic with respect
to its tests.

## Installation

Gene depends on :

- `dune` (>= 2.0, for building)
- `menhir`
- `pprint`
- `z3`

Use `make` to build, then `make test` to run the test suite.

## Example

As an example, we run gene on `examples/max_adhoc.imp` :

```
max (x, y) {
  if (x == 5 && y == 2) {
    return 5
  } else if (x == 3 && y == 4) {
    return 4
  } else {
    return 0
  }
}

test () {
  assertEquals(max (5, 2), 5);
  assertEquals(max (3, 4), 4)
}
```

```console
$ ./gene.exe examples/max_adhoc.imp
assertEquals (max (5, 2), 5)  genericity: 0
assertEquals (max (3, 4), 4)  genericity: 0
```

For each test in the source code, gene outputs a genericity flag (0 or 1),
indicating if gene considers the code to be generic with respect to the test.
