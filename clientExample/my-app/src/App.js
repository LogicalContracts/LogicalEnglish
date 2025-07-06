import React, { useState } from 'react';
import axios from 'axios';

const SERVER_URL_BASE = "http://localhost:3052";
const SERVER_URL = SERVER_URL_BASE + "/leapi";
const MY_TOKEN = "myToken123";
//const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};
//const axiosConfig = {headers:{'Access-Control-Allow-Origin':'*', 'Sec-Fetch-Mode': 'cors', 'Sec-Fetch-Dest': SERVER_URL}};


function App() {
  const [operation, setOperation] = useState('answer'); 
  const [document, setDocument] = useState('');
  const [filename, setFilename] = useState('');
  const [theQuery, setTheQuery] = useState('');
  const [scenario, setScenario] = useState('');
  const [response, setResponse] = useState(null);

  const handleSubmit = async (event) => {
    event.preventDefault();

    const data = {
      operation: operation, 
      token: MY_TOKEN,
      file: filename,
      document: document,
      theQuery: theQuery,
      facts: '', 
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

// Add this function to handle clicking on box elements to toggle nested lists
const handleBoxClick = (event) => {
  const clickedElement = event.target;
  if (clickedElement.classList.contains('box')) {
    const nestedList = clickedElement.parentElement.querySelector('.nested');
    if (nestedList) {
      nestedList.classList.toggle('active');
      clickedElement.classList.toggle('check-box');
    }
  }
};

// Component to render individual explanations
const ExplanationRenderer = ({ explanation }) => {
  return (
    <ul 
      dangerouslySetInnerHTML={{ __html: explanation }}
      onClick={handleBoxClick}
    />
  );
};

// Component be updated to embed html content safely
// This will render the explanation as HTML, allowing for nested lists and other HTML elements
return (
  <div>
    <input type="text" placeholder="Operation" value={operation} onChange={(e) => setOperation(e.target.value)} />
    <input type="text" placeholder="Filename" value={filename} onChange={(e) => setFilename(e.target.value)} />
    <textarea placeholder="LE Document" value={document} onChange={(e) => setDocument(e.target.value)} />
    <input type="text" placeholder="Query" value={theQuery} onChange={(e) => setTheQuery(e.target.value)} />
    <input type="text" placeholder="Scenario" value={scenario} onChange={(e) => setScenario(e.target.value)} />
    <button onClick={handleSubmit}>Answer</button>
    
    {response && response.results && (
      <div>
        {response.results.map((result, index) => (
          <div key={index} style={{ marginBottom: '20px', border: '1px solid #ccc', padding: '10px' }}>
            <h3>Result {index + 1}</h3>
            <p><strong>Answer:</strong> {result.answer}</p>
            <div>
              <strong>Explanation:</strong>
              <ExplanationRenderer explanation={result.explanation} />
            </div>
          </div>
        ))}
      </div>
    )}
    
    {/* Keep the raw JSON display if you still want it */}
    {response && <pre style={{ marginTop: '20px' }}>{JSON.stringify(response, null, 2)}</pre>}
  </div>
);
}
export default App;

// Use the following lines if you want to use the original form without the explanation rendering
//   return (
//     <div>
//       <input type="text" placeholder="Operation" value={operation} onChange={(e) => setOperation(e.target.value)} />
//       <input type="text" placeholder="Filename" value={filename} onChange={(e) => setFilename(e.target.value)} />
//       <textarea placeholder="LE Document" value={document} onChange={(e) => setDocument(e.target.value)} />
//       <input type="text" placeholder="Query" value={theQuery} onChange={(e) => setTheQuery(e.target.value)} />
//       <input type="text" placeholder="Scenario" value={scenario} onChange={(e) => setScenario(e.target.value)} />
//       <button onClick={handleSubmit}>Answer</button>
//       {response && <pre>{JSON.stringify(response, null, 2)}</pre>}
//     </div>
//   );
// }

