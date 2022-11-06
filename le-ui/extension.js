//import axios from 'axios';
const axios = require('axios');
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

const SERVER_URL = "http://localhost:3050/taxkbapi";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";

const LETest = `
the target language is: taxlog.

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

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');
//import vscode from 'vscode';

//console.log(vscode.window)

let myStatusBarItem; 
let leQuery = '';
let leWebViewPanel;
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
	let disposable = vscode.commands.registerCommand('le-ui.helloWorld', function () {
		// The code you place here will be executed every time your command is executed

		// Display a message box to the user
		vscode.window.showInformationMessage('Hello World from LE UI!');
	});

	context.subscriptions.push(disposable);

	let newcommand = vscode.commands.registerCommand('le-ui.run', function () {
		// The code you place here will be executed every time your command is executed
		leWebViewPanel = vscode.window.createWebviewPanel('ViewPanel','LE Greetings', {preserveFocus: true, viewColumn: 1});
		// leWebViewPanel.webview.postMessage(
		// 	leWebViewPanel.webview.postMessage({
		// 		type: 'update',
		// 		text: 'Hello, friend of LE!',
		// 	}))

		leWebViewPanel.webview.html = "Hello, friend of LE!"

		// Display a message box to the user
		vscode.window.showInformationMessage('LE runs!');
	});

	context.subscriptions.push(newcommand);

	let newInputcommand = vscode.commands.registerCommand('le-ui.query', function () {
		// The code you place here will be executed every time your command is executed
		leWebViewPanel = vscode.window.createWebviewPanel('ViewPanel','LE Answers', {preserveFocus: true, viewColumn: 1});

		// Display a message box to the user
		vscode.window.showInputBox().then((value) => {leQuery=value})
		console.log('Querying', leQuery)

		// processQuery = new vscode.ProcessExecution('ls')
		// console.log('Process', processQuery.process.valueOf)
		main()

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
    return (await axios.post(SERVER_URL,{
        token:MY_TOKEN, operation: "loadFactsAndQuery", 
        sessionModule:sessionModule,
        facts: facts, goal:goal, vars:vars
        }, axiosConfig)).data;
}

async function main(){
    console.log("LOGICAL ENGLISH:");
    //console.log(LETest);

    // console.log("Translating to PROLOG...");

    // var result = await axios.post(SERVER_URL,{
    //     token:MY_TOKEN, operation: "le2prolog", 
    //     le: LETest
    //     }, axiosConfig);
    //console.log("Overall result:"); console.log(JSON.stringify(result.data,null,4));

    //console.log(`\n\nPROLOG predicates for KB ${result.data.kb}:`);
    //console.log(result.data.predicates);
    //console.log("\nThe PROLOG test examples:");
    //for (var example of result.data.examples){
    //    console.log(` Example ${example.name}:`);
    //    for (var scenario of example.scenarios){
    //        console.log("% for the following clauses this goal must succeed: "+scenario.assertion);
    //        console.log(scenario.clauses);
    //    }
    //}
    //console.log("\nThe PROLOG clauses:");
    // console.log(result.data.prolog);

	//leWebViewPanel.webview.html = result.data.prolog;

    // console.log("\nNow loading LE from a server file:");

    // var result2 = await loadFile('/Users/mc/git/LogicalEnglish/moreExamples/citizenship.le');

    // console.log("Overall result 2:"); console.log(JSON.stringify(result2,null,4));

 	console.log("\nNow loading LE from a client string:");

    var result3 = await loadString(LETest);

    // console.log("Overall result 3:"); console.log(JSON.stringify(result3,null,4));

    var result4 = await loadFactsAndQuery(result3.sessionModule, [
             "is_a_British_citizen_on('Alice','2021-10-09')", 
             "is_born_in_on('John','the_UK','2021-10-09')", // HACK: 'the_UK'....
             "is_the_mother_of('Alice','John')",
             "is_after_commencement('2021-10-09')"
         ],
         "acquires_British_citizenship_on(Person,Date)",
         ["Person","Date"]
    );
    //console.log("Overall result 4:"); console.log(JSON.stringify(result4,null,4));
	let success = await vscode.commands.executeCommand('workbench.action.splitEditor');

	leWebViewPanel.webview.html = JSON.stringify(result4,null,4);

}

module.exports = {
	activate,
	deactivate
}
