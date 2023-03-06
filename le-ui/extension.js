// @ts-nocheck
//import axios from 'axios';
const axios = require('axios');
var $ = require( "jquery" );
const Pengine = require('./pengines');
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

const SERVER_URL = "http://localhost:3050/taxkbapi";
var PENGINE_URL = "http://localhost:3050/pengine";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";

// The module 'vscode' contains the VS Code extensibility APIServerURL
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
//import vscode from 'vscode';
//console.log(vscode.window)

let myStatusBarItem; 
let leWebViewPanel;
let prologWebViewPanel;
var currentFile = '';
var currentQuery = '';
var currentScenario = '';
var currentAnswer = '';
var currentMore = false; 
var currentExplanation = '';
var runningPengine; 

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "le-ui" is now active!');

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with  registerCommand
	// The commandId parameter must match the command field in package.json
	// let disposable = vscode.commands.registerCommand('le-ui.helloWorld', function () {
	// 	// The code you place here will be executed every time your command is executed

	// 	// Display a message box to the user
	// 	vscode.window.showInformationMessage('Hello World from LE UI!');
	// });

	// context.subscriptions.push(disposable);

	// let newcommand = vscode.commands.registerCommand('le-ui.run', function () {
	// 	// The code you place here will be executed every time your command is executed
	// 	leWebViewPanel = vscode.window.createWebviewPanel('ViewPanel','LE Greetings', {preserveFocus: true, viewColumn: 1});
	// 	// leWebViewPanel.webview.postMessage(
	// 	// 	leWebViewPanel.webview.postMessage({
	// 	// 		type: 'update',
	// 	// 		text: 'Hello, friend of LE!',
	// 	// 	}))

	// 	leWebViewPanel.webview.html = "Hello, friend of LE!"

	// 	// Display a message box to the user
	// 	vscode.window.showInformationMessage('LE runs!');
	// });

	// context.subscriptions.push(newcommand);

	let newInputcommand = vscode.commands.registerCommand('le-ui.query', function () {
		main(context)
	});

	context.subscriptions.push(newInputcommand);

	// register a command that is invoked when the status bar
	// item is selected
	const myCommandId = 'le-ui.showSelectionCount';
	context.subscriptions.push(vscode.commands.registerCommand(myCommandId, () => {
		const n = getNumberOfSelectedLines(vscode.window.activeTextEditor);
		console.log(`Yeah, ${n} line(s) selected... Keep going!`);
		vscode.window.showInformationMessage(`Yeah, ${n} line(s) selected... Keep going!`);
	}));

	// create a new status bar item that we can now manage
	myStatusBarItem = vscode.window.createStatusBarItem('LE', vscode.StatusBarAlignment.Right, 100);
	myStatusBarItem.command = myCommandId;
	context.subscriptions.push(myStatusBarItem);

	// register some listener that make sure the status bar 
	// item always up-to-date
	context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(updateStatusBarItem));
	context.subscriptions.push(vscode.window.onDidChangeTextEditorSelection(updateStatusBarItem));

	// update status bar item once at start
	updateStatusBarItem();

	const serverURL = vscode.workspace.getConfiguration().get('conf.view.url');
	//console.log('configuration', serverURL)
	PENGINE_URL = serverURL;

}

// This method is called when your extension is deactivated
function deactivate() {}

function updateStatusBarItem() {
	const n = getNumberOfSelectedLines(vscode.window.activeTextEditor);
	if (n > 0) {
		myStatusBarItem.text = `LE$(megaphone) ${n} line(s) selected`;
		myStatusBarItem.show();
	} else {
		myStatusBarItem.hide();
	}
}

function getNumberOfSelectedLines(editor) {
	let lines = 0;
	if (editor) {
		lines = editor.selections.reduce((prev, curr) => prev + (curr.end.line - curr.start.line), 0);
	}
	return lines;
}

async function runPengine(filename, le_string, query, scenario) {

	runningPengine = new Pengine({ server: PENGINE_URL,  //authorization:"digest(jacinto, '123')", //authenticate:true, authorization:"basic(jacinto, 123)", 
		//src_text: le_string, 
		oncreate: handleCreate,
		//onprompt: handlePrompt,
		onoutput: handleOutput,
		onsuccess: showAnswer,
		onfailure: reportFailure
	});

	function handleCreate() {
		//runningPengine.ask('member(Answer, [1,2,3])',[]);
		//pengine.ask('assert(parsed)', []);
		//pengine.ask('le_input:pre_is_type(X)', []); 
		//pengine.ask('assert(parsed), show(prolog)');
		//pengine.ask('le_input:dict(A,B,C)'); 
		//runningPengine.ask(`le_taxlog_translate( ${le_string}, File, BaseLine, Terms)`);
		//console.log("parse_and_query("+filename+", "+le_string+", "+query+", with("+scenario+"), Answer, Explanation)");
		runningPengine.ask("le_answer:parse_and_query_and_explanation("+filename+", "+le_string+", "+query+", with("+scenario+"), Answer)");
		//runningPengine.ask("parse_and_query(1, 2, 3, with(4), Answer)", []);
		//pengine.ask('listing');
		//console.log('answer', this);
	}
	// function handlePrompt() {
	// 	runningPengine.input(prompt(this.data));
	// }
	function handleOutput() {
		//$('#out').html(this.data);
		//console.log('pengine answering:', this.data, ' are there more? ', this.more)
	}
	function showAnswer() {
		currentAnswer = this.data[0].Answer; // pick the first answer only
		//currentExplanation = this.data[0].Explanation; 
		currentMore = this.more
		console.log('Answer from pengine', this.id, ' is ', this.data, ' more? ', this.more);
		leWebViewPanel.webview.postMessage({ command: 'answer', text: currentAnswer });
	}
	function reportFailure() {
		console.log('pengine', this.id, 'failed', this); 
	}

}

async function main(context){
	//let success = await vscode.commands.executeCommand('workbench.action.splitEditor');

	var source = vscode.window.activeTextEditor.document.getText();
	var filename = vscode.window.activeTextEditor.document.fileName;
	currentFile = filename.substring(filename.lastIndexOf('/')+1, filename.lastIndexOf('.'));

	// Display a message box to the user
	//vscode.window.showInputBox().then((value) => {leQuery=value})

	prologWebViewPanel = vscode.window.createWebviewPanel('ViewPanel','LE -> Prolog', {preserveFocus: true, viewColumn: 2});

	var translated = await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "le2prolog", 
        le: source
        }, axiosConfig);

	console.log('Translation le to prolog', translated)

	prologWebViewPanel.webview.html = getWebviewPrologContent(translated.data.prolog, filename);

	prologWebViewPanel.webview.postMessage({ command: 'refactor' });

	// Create GUI
	leWebViewPanel = vscode.window.createWebviewPanel('ViewPanel',
		'LE Answers', {preserveFocus: true, viewColumn: 2, }, {enableScripts: true});
	// Load source document on the server
	//var loaded = await loadString(source);

	let le_string = 'en(\"'+source+'\")'

	// Open GUI panel and wait for the query
	leWebViewPanel.webview.html = getWebviewLEGUI()

	//leWebViewPanel.webview.postMessage({ command: 'answer', text: 'hello' });

	// Handle messages from the webview
	leWebViewPanel.webview.onDidReceiveMessage(
        message => {
          switch (message.command) {
            case 'alert':
              	vscode.window.showErrorMessage(message.text);
	  			//leWebViewPanel.webview.postMessage({ command: 'answer', text: 'hello' });
              	return;
			case 'query':
				currentQuery = message.text;
				console.log('Query being processed', currentQuery)
				runPengine(currentFile, le_string, currentQuery, currentScenario)      
				return
			case 'scenario':
				currentScenario = message.text;
				console.log('Scenario being processed', currentScenario)
			 	return
			case 'next':
			 	if(runningPengine&&currentMore) currentAnswer = JSON.stringify(runningPengine.next());
			 	else {
			 		console.log("No more answers");
			 		currentAnswer = "No more"
			 	}
				leWebViewPanel.webview.postMessage({ command: 'answer', text: currentAnswer });
				return
          }
        },
        undefined,
        context.subscriptions
      );

}

function getWebviewPrologContent(prolog, filename) {

	return `<!DOCTYPE html>
  <html lang="en">
  <head>
	  <meta charset="UTF-8">
	  <meta name="viewport" content="width=device-width, initial-scale=2.0">
	  <title>${filename} in Prolog</title>
  </head>
  <body>
	  % from ${filename}
	  <pre>${prolog}</pre>    
  </body>
  </html>`;
  }

function getWebviewLEGUI() {
	return `<!DOCTYPE html>
  <html lang="en">
  <head>
	  <meta charset="UTF-8">
	  <meta name="viewport" content="width=device-width, initial-scale=1.0">
	  <title>Logical English GUI</title>
	  <style>
	  ul, #myUL, #myUL2 {
		list-style-type: none;
	  }

	  #myUL {
		margin: 0;
		padding: 0;
	  }

	.box {
	cursor: pointer;
	-webkit-user-select: none; /* Safari 3.1+ */
	-moz-user-select: none; /* Firefox 2+ */
	-ms-user-select: none; /* IE 10+ */
	user-select: none;
	}

	.box::before {
		content: "\\2610";
		color: black;
		display: inline-block;
		margin-right: 6px;
	  }

	  .leaf {
		cursor: pointer;
		-webkit-user-select: none; /* Safari 3.1+ */
		-moz-user-select: none; /* Firefox 2+ */
		-ms-user-select: none; /* IE 10+ */
		user-select: none;
	  }
	  
	  .leaf::before {
		content: "\\2618";
		color: black;
		display: inline-block;
		margin-right: 6px;
	  }
	  
	  .check-box::before {
		content: "\\2611"; 
		color: dodgerblue;
	  }
	  
	  .nested {
		display: none;
	  }
	  
	  .active {
		display: block;
	  }	  
	  </style>
  </head>
  <body>>
	  <h2>Logical English GUI</h2>
	  <label>Query</label><br>
	  <textarea placeholder="Enter some query" name="query" /></textarea> <br>
	  <label>Scenario</label><br>
	  <input id="scenario" placeholder="Enter some scenario" name="scenario" /> <br><br>
	  <!-- query <p id="values"></p>
	  with scenario <p id="scene"></p> <br>
	  -->
	  
	  <button id="run">Run</button><button id="next">Next</button><br><br>
	  <label>Answers</label><br><br><br>
	  <!-- h4 id="answer0"></h4><br><br>
	  -->

	  <ul id="myUL">		
	  </ul>  	
	  
	  <br>
	  
	  <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js"></script>
      <script src="https://pengines.swi-prolog.org/pengine/pengines.js"></script>

	  <script>
	  	const log3 = document.getElementById('myUL');

		function switchNext() {
			  document.getElementById('next').style.visibility = 'visible';
		}

		function toggling() {
			var toggler = document.getElementsByClassName("box");
			var i;
	
			for (i = 0; i < toggler.length; i++) {
				toggler[i].addEventListener("click", function() {
					this.parentElement.querySelector(".nested").classList.toggle("active");
					this.classList.toggle("check-box");
				});
			}
		}
		
		(function() {
			const vscode = acquireVsCodeApi();
			//const query = document.getElementById('input-query');
			  
			const input = document.querySelector('textarea');
			const scenario = document.querySelector('input');
			//const log = document.getElementById('values');
			//const log2 = document.getElementById('scene');
			document.getElementById('next').style.visibility = 'hidden';

			var tempQuery = '';
			var tempScenario = '';

			input.addEventListener('input', updateValueQuery);
			scenario.addEventListener('input', updateValueScenario);

			function updateValueQuery(e) {
  					//log.textContent = e.target.value;
					tempQuery = e.target.value; 
			}

			function updateValueScenario(e) {
					//log2.textContent = e.target.value;
					tempScenario = e.target.value; 
			}

			const button = document.getElementById('run');

			button.addEventListener('click', (event) => {	
				//button.textContent = \`Running: \${event.detail}\`;
								
				vscode.postMessage({
					command: 'scenario',
					text:  tempScenario
				})				
				vscode.postMessage({
					command: 'query',
					text:  tempQuery
				})
				vscode.postMessage({
					command: 'alert',
					text:  'Query: '+tempQuery + ' with Scenario ' + tempScenario +' '+ button.textContent
				})
			});

			const next = document.getElementById('next');

			next.addEventListener('click', (event) => {
							
				vscode.postMessage({
					command: 'next',
					text: next.textContent
				})
			});
		}());

		window.addEventListener('message', event => {

            const message = event.data; // The JSON data our extension sent

            switch (message.command) {
                // case 'refactor':
                //     log3.textContent = Math.ceil(100 * 0.5);
                //     break;
				case 'answer':
					//log3.textContent = message.text;
					log3.innerHTML = message.text; 
					toggling();
					switchNext();
					return; 
            }
        });
	  </script>

  </body>
  </html>`;  
}

module.exports = {
	activate,
	deactivate
}
