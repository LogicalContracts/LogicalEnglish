// src/api.js

import axios from 'axios';

const SERVER_URL = "http://localhost:3050/taxkbapi";
//const SERVER_URL = "https://le.logicalcontracts.com/taxkbapi";
const MY_TOKEN = "myToken123";
//const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};
const axiosConfig = {headers:{'Access-Control-Allow-Origin':'*'}};

export async function loadFile(ServerFile){
    return (await axios.post(SERVER_URL, {
        token: MY_TOKEN, operation: "load", 
        file: ServerFile
    }, axiosConfig)).data;
}

export async function loadString(LE){
    return (await axios.post(SERVER_URL, {
        token: MY_TOKEN, operation: "load", 
        le: LE
    }, axiosConfig)).data;
}

// TODO: variant using template argument names as object fields/keys
export async function loadFactsAndQuery(sessionModule, facts, goal = 'true', vars = []){
    return (await axios.post(SERVER_URL, {
        token: MY_TOKEN, operation: "loadFactsAndQuery", 
        sessionModule: sessionModule,
        facts: facts, goal: goal, vars: vars
    }, axiosConfig)).data;
}
