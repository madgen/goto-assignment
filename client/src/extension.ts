// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { execSync, ExecException } from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;
let outputChannel: vscode.OutputChannel;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	outputChannel = vscode.window.createOutputChannel('Goto-Assignment');
	outputChannel.show(true);
	outputChannel.appendLine('Activating "vscode-goto-assignment"...');

	let stdout: Buffer;
	try {
		stdout = execSync(`find ${__dirname}/../../server -iname goto-assignment-exe -type file | tail -n1`);
	} catch {
		outputChannel.appendLine("There was a problem finding the LSP server executable.");
		return;
	}

	const execPath = path.normalize(stdout.toString('utf-8').replace(/\n+$/, ''));
	outputChannel.appendLine(`LSP server executable path: "${execPath}"`);

	const serverOptions: ServerOptions = {
		run: {
			command: execPath,
			transport: TransportKind.stdio,
			args: [],
		},
		debug: {
			command: execPath,
			transport: TransportKind.stdio,
			args: [],
		},
	};

	const clientOptions: LanguageClientOptions = {
		// Register the server for While documents
		documentSelector: [{ scheme: 'file', language: 'While' }],
		outputChannel: outputChannel,
	};

	client = new LanguageClient(
		'vscode-goto-assignment',
		'Goto-Assignment',
		serverOptions,
		clientOptions
	);

	client.start();
}

// This method is called when your extension is deactivated
export function deactivate() {
	if (client) {
		client.stop;
	}
}
