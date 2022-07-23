const SERVER_URL = "http://localhost:3050/taxkbapi";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";

import axios from 'axios';
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

const LE = `
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
    // console.log("LOGICAL ENGLISH:");
    // console.log(LE);

    // console.log("Translating to PROLOG...");

    // var result = await axios.post(SERVER_URL,{
    //     token:MY_TOKEN, operation: "le2prolog", 
    //     le: LE
    //     }, axiosConfig);
    // console.log("Overall result:"); console.log(JSON.stringify(result.data,null,4));

    // console.log(`\n\nPROLOG predicates for KB ${result.data.kb}:`);
    // console.log(result.data.predicates);
    // console.log("\nThe PROLOG test examples:");
    // for (var example of result.data.examples){
    //     console.log(` Example ${example.name}:`);
    //     for (var scenario of example.scenarios){
    //         console.log("% for the following clauses this goal must succeed: "+scenario.assertion);
    //         console.log(scenario.clauses);
    //     }
    // }
    // console.log("\nThe PROLOG clauses:");
    // console.log(result.data.prolog);

    // console.log("\nNow loading LE from a server file:");

    // var result2 = await loadFile('/Users/mc/git/LogicalEnglish/moreExamples/citizenship.le');

    // console.log("Overall result 2:"); console.log(JSON.stringify(result2,null,4));

    console.log("\nNow loading LE from a client string:");

    var result3 = await loadString(LE);

    console.log("Overall result 3:"); console.log(JSON.stringify(result3,null,4));

    var result4 = await loadFactsAndQuery(result3.sessionModule, [
            "is_a_British_citizen_on('Alice','2021-10-09')", 
            "is_born_in_on('John','the_UK','2021-10-09')", // HACK: 'the_UK'....
            "is_the_mother_of('Alice','John')",
            "is_after_commencement('2021-10-09')"
        ],
        "acquires_British_citizenship_on(Person,Date)",
        ["Person","Date"]
    );
    console.log("Overall result 4:"); console.log(JSON.stringify(result4,null,4));

}
main();
