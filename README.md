# Go to ASsignment

This is the proof-of-concept implementation of Go to Assignment code navigation
to go with [the associated blog
post](https://dodisturb.me/2023-08-09-Goto-Assignment.html). It comes with an
LSP server implementation under `server` and a VSCode extension under `client`.

To run the demo,
1. run `stack build` under the `server` directory
2. open `client` in VSCode
3. press `F5` or `Run > Start Debugging`. Inside the debugging VS Code window,
   the extension will be loaded.
4. Navigate to one of the `.while` files under `server/test/cases`
5. start using go to definition action by command clicking (or equivalent in
   your VS Code setup).

If you want to experiment with your own programs either change the test files
or open a new file and set the language to `While` (bottom right corner).