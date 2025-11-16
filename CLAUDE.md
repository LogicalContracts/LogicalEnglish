# The "Logical English" (LE) language
Logical English (from now on referred as "LE") is a small subset of English, and a syntactic sugar for pure PROLOG. A LE program represents a piece of regulatory text - legislation,  contract, legal opinion - so that it can be queried for logical consequences.
Its syntax is described in @le_syntax.md. Some syntax additions:
* do not use double quotes (") anywhere in the program unless escaped
* templates must have at least one variable
* Arithmetic expressions are written Result = Expression
For each LE program, represented in a file with extension ".le", there should also exist a file with the same name and extension ".le.tests", containing:
* the expected answers for all queries and scenarios, as PROLOG facts expected(QueryName,ScenarionName,ScenarioSentenceDoubleQuotedStrings).
* it must contain at least one expected fact for each top level predicate
## LE Examples
Examples of LE programs and their expected answers can be found in @moreExamples/

# How to build a LE program
* Analyse the given regulatory text to understand its purpose, the predicates (templates) it defines, and what rules define the truth of those predicates. Also extract main types of arguments for those predicates, so that a small ontology of types can be built if needed.
* Extract any examples from the text, if any, illustrating application of the regulatory text to concrete use cases; each use case should map to a LE scenario (set of predicate fact sentences) plus queries to the main predicates (sentences asking an interesting  question to the regulatory text), and also to the expected answers for each query+scenario. 
* If the given text contains no examples, summarise the text in a short sentence S, search the web with "examples for S", and collect examples from the top page
* before each expected(..) answers fact in the .le.tests file you MUST put a PROLOG comment with the provenance of the example query, the scenario and the expected answers
* draft the LE program with the templates, ontology if any, rules, scenarios and queries; draft also the .le.tests file with expected answers for all queries and scenarios; for all queries, there should be at least one expected answer in some scenario
* Make sure rules use variables, because concrete objects/entities should be provided via scenarios instead; also, comparisons among numbers or dates need to be written with PROLOG operators, instead of comparative adjectives
* Keep the main fragments of the original text as comments, just before the LE rules that represent them
* The test and debug and edit it repeatedly as needed, until:
** all expected answers in the .le.tests file are obtained correctly 
** there are no warning messages
* Do not finish building without all tests running as expected. 
* ALL warnings and errors MUST be fixed. ALL tests MUST succeed.
* Double-check: the new LE program must have neither warnings nor errors, and its tests must all succeed
# How to test a LE program
* Execute the LE verify command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=verify <PROGRAM>
If the program is working as expected, this command will output "ALL GOOD :-)" at the end; otherwise there will be errors and/or warning messages, meaning that the program needs further editing
# How to debug a LE program
React to the errors and warnings produced by the LE verify command. First edit the program as follows, then test it again:
## Missing template for 'sentence'
Generate a template for the sentence and add it to the program 
## Rule without variables
Rules should not refer concrete data, which should be in scenarios; predicates in rules are mostly to refer to variables.
## Predicate is not tested by any query
Ask the user to provide a query for the predicate, as well as expected answers for all scenarios
## Undefined predicate
The predicate must either have a rule defining it, or there must be a scenario with a fact sentence for the predicate. Try to obtain this from the given regulatory text, or perform a web search
## Missing expected result for query <Q> with scenario <S>
Ask the user
## Test failure in scenario <S> for query <Q>
This includes a second line with 
    expected ExpectedAnswers got ComputedAnswers
Be creative and edit the program to fix this.
## Non terminating goal
Look for uncontrolled recursions in the program and fix them
# How to run an arbitrary query and scenario on a program
* Execute the LE query command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=query --query="<QUERY_SENTENCE>" --scenario="<SCENARIO_SENTENCES>" myProgram.le
# How to find the top level predicate templates
* Execute command: /Applications/SWI-Prolog9.3.33-1.app/Contents/MacOS/swipl le_command.pl --command=top_predicates  myProgram.le