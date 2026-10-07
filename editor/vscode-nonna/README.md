# nonna VSCode extension (minimal)

What you get:

- **Diagnostics** on open/edit/save: every function strongly resembling another
  function in the workspace gets an Information squiggle on its first line —
  > `avg` is similar to `mean` (util.rs:1) — jaccard 1.00, containment 1.00

  with matches as expandable related locations in the Problems panel.
  Positions reflect unsaved buffers, including related locations in other
  open documents.
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

- The workspace is indexed in the background at startup. Editor changes
  update the index immediately. Use "nonna: Reindex Workspace" for external
  edits or file additions/deletions outside the editor.
- Matches are reported per function against the whole indexed workspace,
  threshold max(jaccard, containment) >= 0.7, with up to five matches.
