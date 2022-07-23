const SERVER_URL = "http://localhost:3050/taxkbapi";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";

import axios from 'axios';
const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};

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

    console.log("\nNow loading LE from a server file:");

    var result2 = await loadFile('/home/jacinto/git/LogicalEnglish/moreExamples/1_cgt_assets_and_exemptions.le');

    console.log("Loaded:"); console.log(JSON.stringify(result2,null,4));

    var result4 = await loadFactsAndQuery(result2.sessionModule, [
        // scenario([(is_about_a_foreign_currency_transaction(the_event):-true),  
        // (is_for_disposal_of(the_event, '1_bitcoin'):-true),  
        //(is_the_owner_of(the_taxpayer, '1_bitcoin'):-true),  
        // (is_a_cryptocurrency('1_bitcoin'):-true),  
        // (exchanges_for_of_on(the_taxpayer, '1_bitcoin', 20000, dollars, 1657843200.0):-true),  
        // (happens_to_on(the_event, '1_bitcoin', 1657843200.0):-true)], true)]).
        "is_about_a_foreign_currency_transaction('the_event')", 
        "is_for_disposal_of('the_event', '1_bitcoin')", 
        "is_a_cryptocurrency('1_bitcoin')", 
        "is_the_owner_of('the_taxpayer', '1_bitcoin')",
        "exchanges_for_of_on('the_taxpayer', '1_bitcoin', 20000, 'dollars', '2022-07-15')",
        "happens_to_on('the_event', '1_bitcoin', '2022-07-15')"
        ],
        "pays_CGT_for_on(Taxpayer, Asset, Date)",
        ["Taxpayer", "Asset", "Date"]
    );
    console.log("Answer to query via API:"); console.log(JSON.stringify(result4,null,4));

}
main();
