const vscode = require('vscode');
const { workspace, window, commands, Uri, Range, Selection } = vscode;
const { LanguageClient } = require('vscode-languageclient/node');

let client;

function activate(context) {
  const config = workspace.getConfiguration('nonna');
  const serverOptions = {
    command: config.get('serverPath'),
    args: ['lsp', '--profile', config.get('profile')],
  };
  const clientOptions = {
    documentSelector: [
      { language: 'rust' },
      { language: 'python' },
      { language: 'javascript' },
      { language: 'typescript' },
      { language: 'go' },
    ],
  };
  client = new LanguageClient('nonna', 'nonna', serverOptions, clientOptions);
  client.start();
  context.subscriptions.push(client);

  // open a related (similar) function in a split editor
  context.subscriptions.push(
    commands.registerCommand('nonna.openAside', async (uri, line) => {
      const doc = await workspace.openTextDocument(
        uri instanceof Uri ? uri : Uri.parse(uri)
      );
      const ed = await window.showTextDocument(doc, {
        viewColumn: vscode.ViewColumn.Beside,
        preserveFocus: false,
      });
      ed.selection = new Selection(line, 0, line, 0);
      ed.revealRange(new Range(line, 0, line, 0), 1 /* InCenter */);
    })
  );

  // virtual docs serving just one function's body (used by the diff view);
  // the path carries the original extension so the diff gets highlighting
  context.subscriptions.push(
    workspace.registerTextDocumentContentProvider('nonna-fn', {
      async provideTextDocumentContent(uri) {
        const q = JSON.parse(uri.query);
        const res = await client.sendRequest('nonna/functionText', {
          textDocument: { uri: q.file },
          position: { line: q.line, character: 0 },
        });
        return (res && res.text) || '';
      },
    })
  );

  const fnUri = (fileUriStr, line, name) => {
    const ext = fileUriStr.slice(fileUriStr.lastIndexOf('.'));
    return Uri.from({
      scheme: 'nonna-fn',
      path: `/${name}${ext}`,
      query: JSON.stringify({ file: fileUriStr, line }),
    });
  };

  // function-body diff: left = yours, right = the similar one
  context.subscriptions.push(
    commands.registerCommand(
      'nonna.diffSimilar',
      async (srcUri, srcLine, srcName, dstUri, dstLine, dstName) => {
        await commands.executeCommand(
          'vscode.diff',
          fnUri(srcUri, srcLine, srcName),
          fnUri(dstUri, dstLine, dstName),
          `${srcName} ↔ ${dstName} (nonna)`
        );
      }
    )
  );

  // lightbulb on flagged functions: diff + open-aside per similar match
  context.subscriptions.push(
    vscode.languages.registerCodeActionsProvider(clientOptions.documentSelector, {
      provideCodeActions(doc, _range, ctx) {
        const actions = [];
        for (const d of ctx.diagnostics) {
          if (d.source !== 'nonna' || !d.relatedInformation) continue;
          const srcName = d.message.split(' is similar')[0];
          let first = true;
          for (const ri of d.relatedInformation) {
            const name = ri.message.split(' — ')[0];
            const diff = new vscode.CodeAction(
              `Diff against \`${name}\``,
              vscode.CodeActionKind.QuickFix
            );
            diff.command = {
              command: 'nonna.diffSimilar',
              title: 'diff',
              arguments: [
                doc.uri.toString(),
                d.range.start.line,
                srcName,
                ri.location.uri.toString(),
                ri.location.range.start.line,
                name,
              ],
            };
            diff.diagnostics = [d];
            diff.isPreferred = first;
            first = false;
            actions.push(diff);

            const open = new vscode.CodeAction(
              `Open similar \`${name}\` to the side`,
              vscode.CodeActionKind.QuickFix
            );
            open.command = {
              command: 'nonna.openAside',
              title: 'open',
              arguments: [ri.location.uri, ri.location.range.start.line],
            };
            open.diagnostics = [d];
            actions.push(open);
          }
        }
        return actions;
      },
    })
  );

  context.subscriptions.push(
    commands.registerCommand('nonna.findSimilar', async () => {
      const editor = window.activeTextEditor;
      if (!editor) {
        window.showInformationMessage('nonna: no active editor');
        return;
      }
      let res;
      try {
        res = await client.sendRequest('nonna/findSimilar', {
          textDocument: { uri: editor.document.uri.toString() },
          position: { line: editor.selection.active.line, character: 0 },
        });
      } catch (e) {
        window.showErrorMessage(`nonna: ${e.message || e}`);
        return;
      }
      if (!res || !res.query) {
        window.showInformationMessage('nonna: cursor is not inside a function');
        return;
      }
      if (res.hits.length === 0) {
        window.showInformationMessage(
          `nonna: nothing similar to \`${res.query}\` in the index`
        );
        return;
      }
      const items = res.hits.map((h) => ({
        label: `$(symbol-function) ${h.name}`,
        description: `jaccard ${h.jaccard.toFixed(2)}  containment ${h.containment.toFixed(2)}`,
        detail: `${workspace.asRelativePath(h.file)}:${h.line_start}`,
        hit: h,
      }));
      const picked = await window.showQuickPick(items, {
        title: `Functions similar to \`${res.query}\``,
        matchOnDescription: true,
        matchOnDetail: true,
      });
      if (picked) {
        const doc = await workspace.openTextDocument(Uri.file(picked.hit.file));
        const ed = await window.showTextDocument(doc);
        const line = Math.max(0, picked.hit.line_start - 1);
        ed.selection = new Selection(line, 0, line, 0);
        ed.revealRange(new Range(line, 0, line, 0), 1 /* InCenter */);
      }
    })
  );
}

function deactivate() {
  return client ? client.stop() : undefined;
}

module.exports = { activate, deactivate };
