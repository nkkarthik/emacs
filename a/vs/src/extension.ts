import * as vscode from "vscode";
import { lineAt } from "./sh";

export function activate(context: vscode.ExtensionContext) {
  let disposable = vscode.commands.registerCommand(
    "k.executeSelectedLine",
    executeSelectedLine
  );
  context.subscriptions.push(disposable);
  disposable = vscode.commands.registerCommand("k.rerun", rerun);
  context.subscriptions.push(disposable);
  disposable = vscode.commands.registerCommand("k.point", point);
  context.subscriptions.push(disposable);
}

async function point() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    return;
  }
  const { name, dir } = uriPathSplit(editor.document.uri);
  const doc = editor.document
  const pos = editor.selection.active
  const wordRange = doc.getWordRangeAtPosition(pos, /[\w\./\-_~$]+/)
  if (!wordRange) {
    vscode.window.showErrorMessage('nothing found at point')
  }
  let word = doc.getText(wordRange).trim()
  let homePath = "/home/knannuru"
  if (dir.includes("/Users")) homePath = "/Users/knannuru"
  // expand homedir
  word = word.replace(/^~(?=\/|$)/, homePath).replace(/\$HOME\b/, homePath)
  const target = word.startsWith('/') ? vscode.Uri.file(word) : vscode.Uri.joinPath(vscode.Uri.file(dir), word)

  // ensure split two
  const tabGroups = vscode.window.tabGroups
  let targetColumn = vscode.ViewColumn.Two
  if (tabGroups.all.length === 1) {
    await vscode.commands.executeCommand('workbench.action.splitEditor')
    targetColumn = vscode.ViewColumn.Two
  }

  // open in split two
  await vscode.window.showTextDocument(target, {
    viewColumn: targetColumn,
    preserveFocus: false,
  })
}

let previous = {term: "", dir: "", command: ""}

async function executeSelectedLine() {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    return;
  }

  const { name, dir } = uriPathSplit(editor.document.uri);

  // remove file extension
  let termName = name?.split('.').slice(0, -1).join('.')
  let term = findTerminalByName(termName);
  if (!term) {
    term = vscode.window.createTerminal({
      name: termName,
      cwd: dir,
    });
  }

  const pos = editor.selection.active;
  const selection = editor.document.getText(editor.selection);
  let text = selection;
  if (!text) {
    text = lineAt(editor.document.getText(), editor.document.offsetAt(pos)) 
    if (text === '') text = editor.document.lineAt(pos.line).text;
  }
  text = text.trim()

  term.show(true); // preserveFocus = true
  // vscode.commands.executeCommand('workbench.action.terminal.scrollToBottom')
  term.sendText(text, true);
  previous = {term: termName!, dir: dir, command: text}

  // Small delay to ensure terminal processes the input
  // await new Promise(resolve => setTimeout(resolve, 100));

  // Focus back to editor
  // await vscode.commands.executeCommand('workbench.action.focusActiveEditorGroup');

  // Move cursor to the next line
  const nextLine = Math.min(pos.line + 1, editor.document.lineCount - 1);
  const newPosition = new vscode.Position(nextLine, 0);
  editor.selection = new vscode.Selection(newPosition, newPosition);
  editor.revealRange(new vscode.Range(newPosition, newPosition));
}

async function rerun() {
  if (previous.term === "") return
  let term = findTerminalByName(previous.term);
  if (!term) {
    term = vscode.window.createTerminal({
      name: previous.term,
      cwd: previous.dir,
    });
  }
  term.show(true); // preserveFocus = true
  term.sendText(previous.command, true);
}

export function deactivate() {}

function uriPathSplit(uri: vscode.Uri) {
  const segs = uri.path.split("/");
  const name = segs.pop(); // remove filename
  const dir = segs.join("/");
  return { name, dir };
}

function findTerminalByName(name?: string) {
  const terms = vscode.window.terminals;
  for (let term of terms) {
    if (term.name == name) {
      return term;
    }
  }
}

