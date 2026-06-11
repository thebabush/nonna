# nonna VSCode extension (minimal)

What you get:

- **Diagnostics** on open/save: every function strongly resembling another
  function in the workspace gets an Information squiggle on its first line —
  > `avg` is similar to `mean` (util.rs:1) — jaccard 1.00, containment 1.00

  with all matches as expandable related locations in the Problems panel.
- **Lightbulb actions** on flagged functions: "Diff against `mean`" (two-pane
  diff of just the two function bodies) and "Open similar `mean` to the side".
- **Command palette**: "nonna: Find Similar Functions" — QuickPick of matches
  for the function under the cursor, jump on select.

## Setup

```sh
cd editor/vscode-nonna
npm install            # pulls vscode-languageclient
```

Point the extension at the binary, in your VSCode `settings.json`:

```json
{ "nonna.serverPath": "/abs/path/to/nonna-v2/_build/default/nonna/cli/main.exe" }
```

Run it either with `F5` from this folder (Extension Development Host), or
install it persistently:

```sh
npx vsce package        # produces nonna-0.0.1.vsix
code --install-extension nonna-0.0.1.vsix
```

## v0 limitations

- The workspace is indexed once when the server starts (a few hundred ms per
  MB of source); edits don't update the index — reload the window to refresh.
- Matches are reported per function against the whole indexed workspace,
  threshold max(jaccard, containment) >= 0.7, best match only.
