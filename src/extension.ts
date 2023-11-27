// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

function initClient() {
	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { command: "tale-lsp", transport: TransportKind.stdio },
		debug: {
			command: "tales-lsp",
			transport: TransportKind.stdio,
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'tales' }]
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'tale-tale-server',
		'Language Server for tales',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
};

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: any) {
  let recreateServer = vscode.commands.registerCommand('tales-lsp.recreate-server', () => {
    deactivate();
    initClient();
  });
  context.subscriptions.push(recreateServer);
  try {
    initClient();
  } catch {};
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
      return undefined;
    }
    return client.stop();
}
