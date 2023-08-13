# Go to Assignment Server

This is the LSP server implementation for the go-to-assignment navigation
exposed through standard LSP definition action.

Usage:
```
stack build # to build
stack test # to run tests
stack exec -- go-to-assignment-exe # starts the LSP server
```

There are some debugging modes activated through various arguments to
`go-to-assignment-exe`. You can find these under `app/Main.hs`.

The proof of concept implementation is for a simple "While" language with
assignments, conditionals, while loops, and simple arithmetic. You can check
out `test/cases` directory for example programs.

The heart of the computation is in `src/GoToDef.hs`. There are three modes
exposed through that file. The interesting one implementing Go to Assignment is
the `SSA` mode. `First` and `Last` are simpler Go to Definition actions that
find the farthest and closest assignment to a variable in the source code
(these are closer to what is found in many LSP servers).
