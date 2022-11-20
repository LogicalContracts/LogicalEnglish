//import axios from 'axios';
const axios = require('axios');
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

const SERVER_URL = "http://localhost:3050/taxkbapi";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";

const LETest = `
the target language is: prolog.

the templates are:
*a person* acquires British citizenship on *a date*.
*a person* is born in *a place* on *a date*,
*a date* is after commencement,
*a person* is the mother of *a person*,
*a person* is the father of *a person*,
*a person* is a British citizen on *a date*,
*a person* is settled in the UK on *a date*,
*a person* says that *a sentence*,
*a person* is authorised to determine fatherhood.

the knowledge base citizenship includes:
a person acquires British citizenship on a date
if the person is born in the UK on the date
and the date is after commencement
and an other person is the mother of the person
    or the other person is the father of the person
and the other person is a British citizen on the date
    or the other person is settled in the UK on the date.

scenario alice is:
John is born in the UK on 2021-10-09.
2021-10-09 is after commencement.
Alice is the mother of John.
Alice is a British citizen on 2021-10-09.
        
query one is:
    
which person acquires British citizenship on which date.
`;

const LETest2 = `
the target language is: prolog. 

the templates are:
*a dragon* smokes.
*a dragon* is a parent of *a dragon*.
*a dragon* is healthy.
*a dragon* is happy.

the knowledge base dragon includes:

a dragon smokes if 
an other dragon is a parent of the dragon
and the other dragon smokes.

A dragon is healthy if it is not the case that
the dragon smokes.

A dragon is happy if for all cases in which 
the dragon is a parent of an other dragon
it is the case that 
the other dragon is healthy.

scenario one is:

bob is a dragon.
alice is a dragon.
alice is a parent of bob.

query happy is:

which dragon is happy.

query healthy is:
which dragon is healthy.`;

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
//import vscode from 'vscode';

//console.log(vscode.window)

let myStatusBarItem; 
let leQuery = '';
let leWebViewPanel;
let prologWebViewPanel;
// let processQuery; 

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

async function loadFile(ServerFile){
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "load", 
        file: ServerFile
        }, axiosConfig)).data;
}

async function loadString(LE){
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "load", 
        le: LE
        }, axiosConfig)).data;
}

// TODO: variant using template argument names as object fields/keys
async function loadFactsAndQuery(sessionModule,facts,goal='true',vars=[]){
	let result 
	console.log('starting loadFactsAndQuery', sessionModule, facts, goal, vars)
	try {
		result = await axios.post(SERVER_URL,{
			token:MY_TOKEN, operation: "loadFactsAndQuery", 
			sessionModule:sessionModule,
			facts: facts, goal:goal, vars:vars
			}, axiosConfig)
		return result.data
	} catch (e) {
		console.log('loadFactAndQuery error', e)
		console.log(`Goal:${goal} with facts:${facts} failed!`)
	}
    return `Goal:${goal} with facts:${facts} failed!`;
}

async function le_answer(sessionModule,facts,goal='true',vars=[]){
	let result 
	try {
		result = await axios.post(SERVER_URL,{
			token:MY_TOKEN, operation: "answeringQuery", 
			sessionModule:sessionModule,
			facts: facts, goal:goal, vars:vars
			}, axiosConfig)
		return result.data
	} catch (e) {
		console.log('loadFactAndAnswer error', e)
		console.log(`Goal:${goal} with facts:${facts} failed!`)
	}
    return `Goal:${goal} with facts:${facts} failed!`;
}

// Adding direct connecction with the LE's query machinery
async function le_answer_(sessionModule,query,scenario){
	let result
	try {
		result = await axios.post(SERVER_URL,{
			token:MY_TOKEN, operation: "answeringQuery", 
			sessionModule:sessionModule,
			query:query,
			scenario:scenario
			}, axiosConfig)
		return result.data
	} catch (e) {
		console.log('loadFactAndAnswer error', e)
		console.log(`Query:${query} with Scenario:${scenario} failed!`)
	}
    return `Query:${query} with scenario:${scenario} failed!`;
}

async function main(context){
	//let success = await vscode.commands.executeCommand('workbench.action.splitEditor');

	var source = vscode.window.activeTextEditor.document.getText();

    //console.log("Translating to PROLOG...", source.toString());

	// Display a message box to the user
	//vscode.window.showInputBox().then((value) => {leQuery=value})
	//console.log('Querying', leQuery)

	//var input = await showInputBox();

    //console.log("Querying LOGICAL ENGLISH:", input);
    //console.log(LETest);

	prologWebViewPanel = vscode.window.createWebviewPanel('ViewPanel','LE -> Prolog', {preserveFocus: true, viewColumn: 2});

	var translated = await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "le2prolog", 
        le: source
        }, axiosConfig);

	console.log('Translation le to prolog', translated)

	prologWebViewPanel.webview.html = getWebviewPrologContent(translated.data.prolog);

	leWebViewPanel = vscode.window.createWebviewPanel('ViewPanel',
		'LE Answers', {preserveFocus: true, viewColumn: 2, }, {enableScripts: true});

	// Handle messages from the webview
	leWebViewPanel.webview.onDidReceiveMessage(
        message => {
          switch (message.command) {
            case 'alert':
              vscode.window.showErrorMessage(message.text);
              return;
          }
        },
        undefined,
        context.subscriptions
      );

    //console.log("Overall result:"); console.log(JSON.stringify(result.data,null,4));

	var result3 = await loadString(source);

	// console.log('loaded', LETest)

	// var result4 = await loadFactsAndQuery(result3.sessionModule, [
	// 	"is_a_parent_of('Alice','Bob')"],
    //     "is_happy(Dragon)",
    //     ["Dragon"]
    // );

	// var result4 = await loadFactsAndQuery(result3.sessionModule, [
	// 	"is_a_parent_of('Alice','Bob')"],
    //     "is_a_parent_of(Parent, Child)",
    //     ["Parent", "Child"]
    // );

	// var result4 = await le_answer(result3.sessionModule, [
	// 	"is_a_parent_of('Alice','Bob')"],
    //     "is_a_parent_of(Parent, Child)",
    //     ["Parent", "Child"]
    // );

	var result4 = await le_answer_(result3.sessionModule, 'happy', 'one');

    //console.log(`\n\nPROLOG predicates for KB ${result.data.kb}:`);
    //console.log(result.data.predicates);
    //console.log("\nThe PROLOG test examples:");>

	//var inter = await showInputBox();

    // console.log("Overall result 3:"); console.log(JSON.stringify(result3,null,4));

    // var result4 = await loadFactsAndQuery(result3.sessionModule, [
    //          "is_a_British_citizen_on('Alice','2021-10-09')", 
    //          "is_born_in_on('John','the_UK','2021-10-09')", // HACK: 'the_UK'....
    //          "is_the_mother_of('Alice','John')",
    //          "is_after_commencement('2021-10-09')"
    //      ],
    //      "acquires_British_citizenship_on(Person,Date)",
    //      ["Person","Date"]
    // );
    // console.log("Overall result 4:"); console.log(JSON.stringify(result4,null,4));

	// leWebViewPanel.webview.html = getWebviewPrologContent(JSON.stringify(result4,null,4));
	leWebViewPanel.webview.html = getWebviewLEGUI(JSON.stringify(result4,null,4))
}

/**
 * Shows an input box using window.showInputBox().
 */
async function showInputBox() {
	const result = await vscode.window.showInputBox({
		value: 'one',
		valueSelection: [2, 4],
		placeHolder: 'For example a query. But not: 123',
		validateInput: text => {
			vscode.window.showInformationMessage(`Validating: ${text}`);
			return text === '123' ? 'Not 123!' : null;
		}
	});
	vscode.window.showInformationMessage(`Got: ${result}`);
	return result
}

function getWebviewPrologContent(prolog) {

	return `<!DOCTYPE html>
  <html lang="en">
  <head>
	  <meta charset="UTF-8">
	  <meta name="viewport" content="width=device-width, initial-scale=2.0">
	  <title>Prolog</title>
  </head>
  <body>
	  <pre>${prolog}</pre>
  </body>
  </html>`;
  }

  function getWebviewLEGUI(answer) {
	return `<!DOCTYPE html>
  <html lang="en">
  <head>
	  <meta charset="UTF-8">
	  <meta name="viewport" content="width=device-width, initial-scale=1.0">
	  <title>Logical English GUI</title>
  </head>
  <body>
	  <h2>Logical English GUI</h2>
	  <label>Query</label><br>
	  <textarea placeholder="Enter some text" name="query" /></textarea> <br>
	  <label>Scenario</label><br>
	  <input id="scenario" placeholder="Enter some text" name="scenario" /> <br>
	  <label>Answers</label><br>
	  <p id="values">
	  <pre>${answer}</pre></p> <br>
	  <p id="scene">here</p> <br>
	  
	  <button>Run</button>

	  <script>
		  (function() {
			  const vscode = acquireVsCodeApi();
			  //const query = document.getElementById('input-query');
			  
			  const input = document.querySelector('textarea');
			  const scenario = document.querySelector('input');
			  const log = document.getElementById('values');
			  const log2 = document.getElementById('scene');

			  var tempQuery = '';
			  var tempScenario = '';

			  input.addEventListener('input', updateValueQuery);
			  scenario.addEventListener('input', updateValueScenario);

			  function updateValueQuery(e) {
  					log.textContent = e.target.value;
					tempQuery = e.target.value; 
				}

			  function updateValueScenario(e) {
					log2.textContent = e.target.value;
					tempScenario = e.target.value; 
			  }

			  const button = document.querySelector('button');

			  button.addEventListener('click', (event) => {	
				//button.textContent = \`Running count: \${event.detail}\`;
				vscode.postMessage({
					command: 'alert',
					text:  'Query: '+tempQuery + ' with Scenario ' + tempScenario +' '+ button.textContent
				})
				});
		  }())
	  </script>
  </body>
  </html>`;  
  }

module.exports = {
	activate,
	deactivate
}
