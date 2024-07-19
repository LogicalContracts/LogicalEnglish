import logo from './logo.svg';
import './App.css';

// src/App.js

import React, { useEffect, useState } from 'react';
import { loadFile, loadFactsAndQuery } from './api';

function App() {
    const [result, setResult] = useState(null);
    const [queryResult, setQueryResult] = useState(null);

    useEffect(() => {
        async function fetchData() {
            try {
                console.log("LOGICAL ENGLISH:");
                console.log("\nNow loading LE from a server file:");

                const result2 = await loadFile('/home/jacinto/git/LogicalEnglish/moreExamples/1_cgt_assets_and_exemptions.le');
                console.log("Loaded:", JSON.stringify(result2, null, 4));
                setResult(result2);

                const result4 = await loadFactsAndQuery(result2.sessionModule, [
                    "is_about_a_foreign_currency_transaction('the_event')",
                    "is_for_disposal_of('the_event', '1_bitcoin')",
                    "is_a_cryptocurrency('1_bitcoin')",
                    "is_the_owner_of('the_taxpayer', '1_bitcoin')",
                    "exchanges_for_of_on('the_taxpayer', '1_bitcoin', 20000, 'dollars', '2022-07-15')",
                    "happens_to_on('the_event', '1_bitcoin', '2022-07-15')"
                ],
                "pays_CGT_for_on(Taxpayer, Asset, Date)",
                ["Taxpayer", "Asset", "Date"]);

                console.log("Answer to query via API:", JSON.stringify(result4, null, 4));
                setQueryResult(result4);
            } catch (error) {
                console.error("Error fetching data:", error);
            }
        }

        fetchData();
    }, []);

    return (
        <div className="App">
               <header className="App-header">
                 <img src={logo} className="App-logo" alt="logo" />
               </header>
            <h1>Logical English API Data</h1>
            <div>
                <h2>Loaded Result</h2>
                <pre>{result && JSON.stringify(result, null, 4)}</pre>
            </div>
            <div>
                <h2>Query Result</h2>
                <pre>{queryResult && JSON.stringify(queryResult, null, 4)}</pre>
            </div>
        </div>
    );
}

export default App;



// function App() {
//   return (
//     <div className="App">
//       <header className="App-header">
//         <img src={logo} className="App-logo" alt="logo" />
//         <p>
//           Edit <code>src/App.js</code> and save to reload.
//         </p>
//         <a
//           className="App-link"
//           href="https://reactjs.org"
//           target="_blank"
//           rel="noopener noreferrer"
//         >
//           Learn React
//         </a>
//       </header>
//     </div>
//   );
// }

// export default App;
