import React, { useState, useEffect, useMemo } from 'react';
import axios from 'axios';

// 1. IMPORT THE JSON DATA
const DATA = [
  {
    "File_Example": "1_cgt_assets_and_exemptions_3",
    "Summary": "This example covers CGT assets and exemptions.",
    "Scenario_ids": [
      "Colin"
    ],
    "Queries": [
      {
        "Query_id": "one",
        "Query_text": "which thing is a CGT asset."
      },
      {
        "Query_id": "two",
        "Query_text": "which asset is a CGT exempt asset."
      },
      {
        "Query_id": "three",
        "Query_text": "which asset is a pre-date CGT asset."
      }
    ]
  },
  {
    "File_Example": "1_net_asset_value_test_3",
    "Summary": "This example covers the maximum net asset value test.",
    "Scenario_ids": [
      "feb 5 2021"
    ],
    "Queries": [
      {
        "Query_id": "andrew",
        "Query_text": "andrew satisfies maximum net asset value test at which date."
      },
      {
        "Query_id": "taxpayer",
        "Query_text": "which taxpayer satisfies maximum net asset value test at which"
      },
      {
        "Query_id": "date",
        "Query_text": "which taxpayer satisfies maximum net asset value test at 2021-4"
      },
      {
        "Query_id": "asset",
        "Query_text": "which asset is a cgt asset."
      }
    ]
  },
  {
    "File_Example": "3_rollover_3",
    "Summary": "This example covers the small business restructure rollover.",
    "Scenario_ids": [
      "Testing",
      "Andrew email Feb 4 2021",
      "Andrew email Feb 4 2021 version 2"
    ],
    "Queries": [
      {
        "Query_id": "one",
        "Query_text": "the small business restructure rollover applies to which event."
      },
      {
        "Query_id": "two",
        "Query_text": "which tax payer is a party of which event."
      }
    ]
  }
];

const SERVER_URL_BASE = "http://localhost:3052";
const SERVER_URL = SERVER_URL_BASE + "/leapi";
const MY_TOKEN = "myToken123";
const MANUAL_FILE_INPUT = 'MANUAL_FILE_INPUT';
//const ApiKey = "..."; // on the server side now
//const axiosConfig = {/*headers:{'Access-Control-Allow-Origin':'*'}*/};
//const axiosConfig = {headers:{'Access-Control-Allow-Origin':'*', 'Sec-Fetch-Mode': 'cors', 'Sec-Fetch-Dest': SERVER_URL}};

// --- Custom Components & Helpers ---
// Component to handle the nested HTML explanation (Keep ExplanationRenderer as-is) ---
const ExplanationRenderer = ({ explanation }) => {
  const handleBoxClick = (event) => {
    const clickedElement = event.target;
    if (clickedElement.classList.contains('box') || clickedElement.tagName === 'LI') {
      const nestedList = clickedElement.parentElement.querySelector('.nested');
      if (nestedList) {
        nestedList.classList.toggle('active');
        clickedElement.classList.toggle('check-box');
      }
    }
  };

  return (
    <ul 
      className="explanation-list"
      dangerouslySetInnerHTML={{ __html: explanation }}
      onClick={handleBoxClick}
    />
  );
};

function App() {
  // --- New State for Stage Control ---
  const [stage, setStage] = useState(1); // 1: File Selection, 2: Query/Scenario Selection/Input, 3: Response Display

  // --- Stage 1 State: File Selection ---
  const [selectedFileOption, setSelectedFileOption] = useState('');
  const [manualFilename, setManualFilename] = useState('');
  const [leDocument, setLEDocument] = useState(''); // Document input moved to Stage 1, always available

  // --- Stage 2 State: Scenario & Query Selection/Input ---
  const [selectedScenario, setSelectedScenario] = useState('');
  const [selectedQueryID, setSelectedQueryID] = useState('');
  const [userinput, setUserInput] = useState(''); // New state for situation/query input

  // --- Other State ---
  // --- Existing State for Submission/Response ---
  const [operation, setOperation] = useState('explain'); // Default operation
  const [response, setResponse] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  // --- Derived State (Memoization) ---
  const finalFilename = useMemo(() => {
    if (selectedFileOption === MANUAL_FILE_INPUT) return manualFilename;
    return selectedFileOption;
  }, [selectedFileOption, manualFilename]);

  const currentFile = useMemo(() => {
    return DATA.find(file => file.File_Example === selectedFileOption);
  }, [selectedFileOption]);
  
  // The query text to be submitted: either selected from list or manually typed
  const finalQueryText = useMemo(() => {
      // If a query from the list is selected, use that
      if (selectedQueryID) return selectedQueryID;
      // Otherwise, leave it empty 
      return '';
  }, [selectedQueryID]);

  // At the top level of your App function:
  useEffect(() => {
    // This runs AFTER the render where the new state is applied
    if (response) {
        console.log("✅ STATE CONFIRMED: Response updated.");
    }
    if (leDocument) {
        console.log("✅ STATE CONFIRMED: leDocument updated. Length:", leDocument.length);
    }
    if (userinput) {
        console.log("✅ STATE CONFIRMED: userinput updated.");
    }
  }, [response, leDocument, userinput]); // Runs when these dependencies change

  // --- Handlers ---

  const handleStage1Next  = async (event) => {
    event.preventDefault();
    setLoading(true);
    setError(null);
    setResponse(null);

    const data = {
      operation: 'examples',
      token: MY_TOKEN,
      file: finalFilename
    };
    console.error("Sending Stage 1 data to server:", data);

    try {
      const apiResponse = await axios.post(SERVER_URL, data, {
        headers: {
          'Content-Type': 'application/json',
          'Access-Control-Allow-Origin':'*'
        }
      });
      console.log("Type of API Data received:", typeof apiResponse.data, apiResponse.data);
      setResponse(apiResponse.data);
      setLEDocument(apiResponse.data.results.document || '');
      setOperation('explain'); // Reset operation after fetching example
      //setLEDocument(apiResponse.data.document); // Optionally pre-fill document from response if needed
      //setLEDocument("Un texto de ejemplo recibido del servidor."); // Placeholder text for testing
    } catch (err) {
      console.error("API Error:", err);
      setError("Failed to get examples from server. Check console for details.");
      setLoading(false);
    } finally {
      setLoading(false);
    }
    console.error("Stage 1: finalFilename =", finalFilename);
    // Stage 1 validation: Must have a file (either selected or manually entered)
    if (finalFilename) {
        setStage(2);
    } else {
        alert("Please select an existing LE Example, or provide a name for an LE Document, to proceed.");
    }
  };
  
  // Handler for when the user wants to go back to file selection
  const handleStage2Back = () => {
    setStage(1);
    // Optionally clear stage 2 selections when going back
    setSelectedScenario('');
    setSelectedQueryID('');
    setUserInput('');
  }

  const handleSubmit = async (event) => {
    event.preventDefault();
    setLoading(true);
    setError(null);
    setResponse(null);
    // setOperation('explain'); // Default operation // Done on the fly below with onChange handlers

    console.error("Stage2: finalFilename ='", finalFilename, "' finalQueryText ='", finalQueryText, "' selectedScenario ='", selectedScenario, "' userinput ='", userinput, "'");

    // if no query or no scenario is provided, fall back to answer_via_llm // Done with onChange handlers
    // if (!finalQueryText || finalQueryText.trim().length === 0 || !selectedScenario || selectedScenario.trim().length === 0) {
    //   setOperation('answer_via_llm');
    // }

    const data = {
      operation: operation,
      token: MY_TOKEN,
      file: finalFilename,
      document: leDocument,
      userinput: userinput,
      theQuery: finalQueryText, // Use the derived finalQueryText
      facts: '',
      scenario: selectedScenario
    };
    console.error("Stage2: Submitting this data to server:", data);

    try {
      const apiResponse = await axios.post(SERVER_URL, data, {
        headers: {
          'Content-Type': 'application/json',
          'Access-Control-Allow-Origin':'*'
        }
      });
      setResponse(apiResponse.data);
      console.error("Stage2: After ", operation, " API Response:", apiResponse.data);
      setOperation('explain'); // Reset operation after submission
      setStage(3); // Move to a final stage to show response
    } catch (err) {
      console.error("API Error:", err);
      setError("Failed to get answer from server. Check console for details.");
      setLoading(false);
    } finally {
      setLoading(false);
    }
  };

  // --- Stage 1 Renderer: File Selection ---
  const renderStage1 = () => (
    <>
      <h2>First, let us define a theme based on an LE Example</h2>
      <div className="selection-grid-stage1">
        <div className="selector-column">
          <select 
            value={selectedFileOption} 
            onChange={(e) => setSelectedFileOption(e.target.value)}
          >
            <option value="">-- Select File or Enter Manually --</option>
            {DATA.map((item) => (
              <option key={item.File_Example} value={item.File_Example}>
                {item.File_Example}
              </option>
            ))}
            <option value={MANUAL_FILE_INPUT}>[Enter File Manually]</option>
          </select>
          <label> -File Example</label>
        </div>

        {selectedFileOption === MANUAL_FILE_INPUT && (
            <div className="selector-column">
                <input
                    type="text"
                    placeholder="Enter file name (e.g., my_new_file.xml)"
                    value={manualFilename}
                    onChange={(e) => setManualFilename(e.target.value)}
                    className="manual-input-field"
                />
                <label> - New Filename</label>
            </div>
        )}
      </div>

      <button 
        onClick={handleStage1Next}
        className="submit-button"
        style={{ width: 'auto', marginTop: '20px' }}
      >
        Continue to Playground »
      </button>
    </>
  );

  // --- Stage 2 Renderer: Scenario/Query Selection or Custom Input ---
  const renderStage2 = () => (
    <>
      <button onClick={handleStage2Back} className="back-button">« Back to File Selection</button>
      <h2>Base Document</h2>
      <div className="config-inputs">
        <label className="full-width-label">
          <textarea 
            className="scrollable-editor"
            value={leDocument} 
            onChange={(e) => setLEDocument(e.target.value)} 
            spellCheck="false" // Optional: prevents red underlines on technical text
            placeholder="Enter the full document text here if no file is selected."
          /> -  LE Document
        </label>
      </div>

      <h2>Second, let us define a scenario and query or simply describe your situation and interests</h2>
      
      {/* Show the selected file context */}
      <p className="context-display">
        Base Filename <b>{finalFilename || '[No File Selected]'}</b>
      </p>

      {/* Conditional UI based on whether a structured file was selected */}
      {currentFile && (
        // --- Option A: Structured Input for Curated Files ---
        <>
          {/* --- Option A: Structured Input for Curated Files --- */}
          <div className="selection-grid-stage2">
            {/* 1. Scenario Selector */}
            <div className="selector-column">
              <select
                value={selectedScenario}
                onChange={(e) => setSelectedScenario(e.target.value)}
              >
                <option value="">-- Select Scenario ID (Optional) --</option>
                {currentFile.Scenario_ids.map((scenarioId) => (
                  <option key={scenarioId} value={scenarioId}>
                    {scenarioId}
                  </option>
                ))}
              </select>
              <label> - Scenario ID</label>
            </div>

            {/* 2. Query Selector */}
            <div className="selector-column">
              <select
                value={selectedQueryID}
                onChange={(e) => {
                  setSelectedQueryID(e.target.value); // selecting by Query_id below
                  setUserInput(''); // Clear manual input when selecting from list
                  setOperation('explain'); // Reset operation when selecting from list
                } }
              >
                <option value="">-- Select Query ID (Optional) --</option>
                {currentFile.Queries.map((query) => (
                  <option key={query.Query_id} value={query.Query_id}>
                    {`${query.Query_id}: ${query.Query_text}`}
                  </option>
                ))}
              </select>
              <label> - Pre-defined Query ID</label>
              <p className="or-divider">OR</p>
            </div>

            {/* 3. Manual Query Input */}
            <div className="selector-column">
              <textarea
                placeholder="Enter your own specific query id text here."
                value={selectedQueryID}
                onChange={(e) => {
                  setSelectedQueryID(e.target.value);
                  setUserInput(''); // Clear manual input when typing 
                  setOperation('explain'); // Reset operation when selecting from list
                } }
                rows="4" />
              <label> - Custom Query ID</label>
            </div>
              <p className="or-divider">OR</p>
          </div>
          {/* --- Option B: Direct Questions --- */}
          <div className="manual-query-input-only">
            <textarea
              className="scrollable-editor"
              placeholder="Enter your question/query and a brief description of the situation."
              value={userinput}
              onChange={(e) => { setUserInput(e.target.value);  
                setSelectedQueryID(''); // Clear selected query when typing
                setSelectedScenario(''); // Clear selected scenario when typing
                setOperation('answer_via_llm');
              } }
              spellCheck="false" // Optional: prevents red underlines on technical text" 
            />
            <label> - Situation/Question Text</label>
         </div>
        </>
      )}

      <button 
        onClick={handleSubmit} 
        disabled={loading || (selectedScenario.trim() === '' && selectedQueryID.trim() === '' && userinput.trim() === '')} 
        className="submit-button"
      >
        {loading ? 'Loading...' : 'Get an Answer »'}
      </button>
    </>
  );

  // --- Stage 3 Renderer: Response Display ---
  const renderStage3 = () => (
    <>
      <button onClick={() => setStage(2)} className="back-button">« Ask a New Question</button>
      <h2>System Response</h2>
      
      {loading && <p>Processing your request...</p>}
      {error && <p className="error-message">Error: {error}</p>}
      
      {response && (
          // ... (Existing response rendering logic)
          console.error("Rendering response:", response),
          <>
            {response.document && setLEDocument(response.document)}
            {response.answer && (
                <div className="result-box">
                    <p><strong>Answer:</strong> {response.answer}</p>
                </div>
            )}
            {Array.isArray(response.results) && response.results.map((result, index) => (
                <div key={index} className="result-box">
                    <h3>Result {index + 1}</h3>
                    <p><strong>Answer:</strong> {result.answer}</p>
                    <div>
                        <strong>Explanation:</strong>
                        <ExplanationRenderer explanation={result.explanation} />
                    </div>
                </div>
            ))}
            {response.translation && <p><strong>This is what I understood from your Input:</strong> {response.translation}</p>}
            </>
      )}
    </>
  );

  // --- Main App Render ---
  return (
    <div className="app-container">
      <h1>Logical English LLM Testing Playground</h1>
      <p>You may play with existings examples of LE documents, defining scenarios and asking queries, or trying your own</p>
      <p>An LLM is provided to let you write descriptions of situations and questions in your own terms (in English).</p>
      
      {stage === 1 && renderStage1()}
      {stage === 2 && renderStage2()}
      {stage === 3 && renderStage3()}
    </div>
  );


  // --- Previous Submission Handler ---
//   const handleSubmit = async (event) => {
//     event.preventDefault();

//     // Use the selected values for the API call    
//     const data = {
//       operation: operation, 
//       token: MY_TOKEN,
//       //gemini_api_key: ApiKey, 
//       file: selectedFile, // Selected File instead of filename
//       document: document,
//       userinput: userinput,
//       theQuery: selectedQueryText, // Selected Query Text
//       facts: '',
//       scenario: selectedScenario // Selected Scenario
//     };

//     try {
//       const response = await axios.post(SERVER_URL, data, {
//         headers: {
//           'Content-Type': 'application/json', 
//           'Access-Control-Allow-Origin':'*' 
//         }
//       });
//       setResponse(response.data);
//     } catch (error) {
//       console.error("API Error:", error);
//       setError("Failed to get answer from server. Check console for details.");
//     } finally {
//       setLoading(false);
//     }
//   };

// // Add this function to handle clicking on box elements to toggle nested lists
// const handleBoxClick = (event) => {
//   const clickedElement = event.target;
//   if (clickedElement.classList.contains('box')) {
//     const nestedList = clickedElement.parentElement.querySelector('.nested');
//     if (nestedList) {
//       nestedList.classList.toggle('active');
//       clickedElement.classList.toggle('check-box');
//     }
//   }
// };

// // Component to render individual explanations
// const ExplanationRenderer = ({ explanation }) => {
//   return (
//     <ul 
//       dangerouslySetInnerHTML={{ __html: explanation }}
//       onClick={handleBoxClick}
//     />
//   );
// };



// Previous code for rendering the UI and response
// This will render the explanation as HTML, allowing for nested lists and other HTML elements
// return (
//   <div>
//     <input type="text" placeholder="Operation" value={operation} onChange={(e) => setOperation(e.target.value)} />
//     <input type="text" placeholder="Filename" value={filename} onChange={(e) => setFilename(e.target.value)} />
//     <textarea placeholder="LE Document" value={document} onChange={(e) => setDocument(e.target.value)} />
//     <textarea placeholder="Description" value={userinput} onChange={(e) => setUserInput(e.target.value)} />
//     <button onClick={handleSubmit}>Answer with LLM</button>
    
//     {response && response.results && (
//       <div>
//         {response.results.map((result, index) => (
//           <div key={index} style={{ marginBottom: '20px', border: '1px solid #ccc', padding: '10px' }}>
//             <h3>Result {index + 1}</h3>
//             <p><strong>Answer:</strong> {result.answer}</p>
//             <div>
//               <strong>Explanation:</strong>
//               <ExplanationRenderer explanation={result.explanation} />
//             </div>
//           </div>
//         ))}
        
//       </div>
//     )}

//     {response && response.translation && <p><strong>This is what I understood from your Input:</strong> {response.translation}</p>}

//     {response && response.answer && <p><strong>Answer:</strong> {response.answer}</p>}
    
//     {/* Keep the raw JSON display if you still want it */}
//     {/* response && <pre style={{ marginTop: '20px' }}>{JSON.stringify(response, null, 2)}</pre> */}
//   </div>
// );
}
export default App;


