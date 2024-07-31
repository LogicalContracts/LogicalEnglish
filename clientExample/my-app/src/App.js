import React, { useState } from 'react';
import axios from 'axios';

const SERVER_URL_BASE = "http://localhost:3050";
const SERVER_URL = SERVER_URL_BASE + "/leapi";
const MY_TOKEN = "myToken123";
//const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};
//const axiosConfig = {headers:{'Access-Control-Allow-Origin':'*', 'Sec-Fetch-Mode': 'cors', 'Sec-Fetch-Dest': SERVER_URL}};


function App() {
  const [document, setDocument] = useState('');
  const [filename, setFilename] = useState('');
  const [theQuery, setTheQuery] = useState('');
  const [scenario, setScenario] = useState('');
  const [response, setResponse] = useState(null);

  const handleSubmit = async (event) => {
    event.preventDefault();

    const data = {
      token: MY_TOKEN,
      file: filename,
      document: document,
      theQuery: theQuery,
      scenario: scenario
    };

    try {
      const response = await axios.post(SERVER_URL, data, {
        headers: {
          'Content-Type': 'application/json', 
          'Access-Control-Allow-Origin':'*' 
        }
      });
      setResponse(response.data);
    } catch (error) {
      console.error(error);
    }
  };

  return (
    <div>
      <input type="text" placeholder="Filename" value={filename} onChange={(e) => setFilename(e.target.value)} />
      <textarea placeholder="LE Document" value={document} onChange={(e) => setDocument(e.target.value)} />
      <input type="text" placeholder="Query" value={theQuery} onChange={(e) => setTheQuery(e.target.value)} />
      <input type="text" placeholder="Scenario" value={scenario} onChange={(e) => setScenario(e.target.value)} />
      <button onClick={handleSubmit}>Answer</button>
      {response && <pre>{JSON.stringify(response, null, 2)}</pre>}
    </div>
  );
}

export default App;
