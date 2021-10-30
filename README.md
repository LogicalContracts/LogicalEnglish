# TaxKB
Prolog based generic knowledge base for tax regulations, including reasoner, editor and other tools.

<!-- vscode-markdown-toc -->
* 1. [Initial sample of examples](#Initialsampleofexamples)
	* 1.1. [LodgeIT](#LodgeIT)
	* 1.2. [AORA](#AORA)
* 2. [TaxLog, the Tax-KB language](#TaxLogtheTax-KBlanguage)
	* 2.1. [Other constructs](#Otherconstructs)
	* 2.2. [Interfacing to external systems](#Interfacingtoexternalsystems)
* 3. [Towards Logical English](#TowardsLogicalEnglish)
	* 3.1. [Direct access to Logical English rendering](#DirectaccesstoLogicalEnglishrendering)
	* 3.2. [To improve the Logical English representation](#ToimprovetheLogicalEnglishrepresentation)
	* 3.3. [Logical English style checking](#LogicalEnglishstylechecking)
	* 3.4. [Technical details](#Technicaldetails)
* 4. [Predicate and web API reference](#PredicateandwebAPIreference)
	* 4.1. [Language processing](#Languageprocessing)
	* 4.2. [Perspective on existing (encoded) Knowledge Pages](#PerspectiveonexistingencodedKnowledgePages)
	* 4.3. [Querying](#Querying)
	* 4.4. [Parsing Logical English](#LEParsing)
* 5. [Installation and deployment](#Installationanddeployment)
	* 5.1. [Quick recipe for a development server](#Quickrecipeforadevelopmentserver)
	* 5.2. [More details](#Moredetails)
	* 5.3. [Where is the knowledge: SWISH storage vs. file system](#Whereistheknowledge:SWISHstoragevs.filesystem)
* 6. [Implementational aspects](#Implementationalaspects)
	* 6.1. [Rendering](#Rendering)
	* 6.2. [Editor](#Editor)
* 7. [Release Notes](#Releases)

<!-- vscode-markdown-toc-config
	numbering=true
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

##  1. <a name='Initialsampleofexamples'></a>Initial sample of examples

The following tax regulation fragments were indicated by each project sponsor; original links and Tax-KB editor links below.

###  1.1. <a name='LodgeIT'></a>LodgeIT

Provided via Andrew Noble email Nov 28, 2020, and earlier:
-  <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/>
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/cgt_concessions_basic_conditions_sb.pl)

- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover/>
	- Bob's [GoogleDoc](https://docs.google.com/document/d/1fj8Cjp_FNKAIXvrUYD52Zpfgo6Ftqbypd8s36mC7CdA/edit?ts=5fc7e58b) analyzing it.
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/cgt_concessions_sb_restructure_rollover.pl)
- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/>
	- Bob's [GoogleDoc](https://docs.google.com/document/d/1wJl_JzZ7tkMMyia3eKdz1XlX6JBhYIcngJTESZQjz6E/edit?ts=5fd3aafc&skip_itp2_check=true)
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/cgt_maximum_net_asset_value.pl)
- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/>
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/cgt_affiliates.pl)
	- Further references (ignored in the first encoding): <https://pointonpartners.com.au/small-business-cgt-passing-the-threshold-tests/>

This was indicated (Dec 10 AN email) for later analysis: <https://www.taxtalks.com.au/small-business-participation-percentage/>


###  1.2. <a name='AORA'></a>AORA
_Some suggestions for simplification and more info in first and second Chris emails, Dec 2, 2020_

- <https://www.gov.uk/guidance/stamp-duty-reserve-tax-reliefs-and-exemptions>
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/stamp_duty_reserve_tax_reliefs.pl)
- <https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040>
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/statutory_residence_test_guidance.pl)
	- Bob's [GoogleDoc](https://docs.google.com/document/d/1nzoGuzPKI375jxtxT38jCEYwuhuZvjSBSHmvxusW0Lg/edit?ts=5fc92d43#heading=h.pzozuu8bp8r0)
- <https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief>
	- Tax-KB [editor](http://demo.logicalcontracts.com:8082/p/research_and_development_tax_reliefs.pl)

##  2. <a name='TaxLogtheTax-KBlanguage'></a>TaxLog, the Tax-KB language

"**TaxLog**" is the "sugared Prolog" language used to encode the knowledge gleaned from regulatory textual sources into *knowledge pages*, forming a logical knowledge graph.

The text in regulatory sources comprises 3 types of fragments: 

- comments (to be effectively ignored); 
- rule ingredients (the main thing)
- examples

The term *knowledge page* denotes a Prolog module that encodes the knowledge in some web (or accessible via URL) page containing the regulatory text. This knowledge excludes instance (specific case) data, to be added in runtime. It is declared with a standard Prolog module declaration:

	:- module('http://myURLBase',[]).

This means that the knowldege page was constructed from the text at that URL. It must be the very first Prolog term in the file.

The logic language used is pure PROLOG (Horn clauses plus a few connectives, excluding cuts) but sugared with some additional operators in the clause bodies, towards readability and expressiveness:

  * ``if``, ``and``, ``or``, as alternatives to :- , ;
  * ``if...must/then..``  and ``if...then...else...``
  * Predicate ``on `` Moment
  * Predicate ``at`` KnowledgePageURL

The "deontic" ``if Condition (it) must (be true that) Obligation`` in a clause body is simply mapped into ``Condition and Obligation or not Condition``. The term "deontic" is being stretched here, cf. for example this explanation on [deonthic vs. alethic](https://www.brsolutions.com/the-two-fundamental-kinds-of-business-rules-where-they-come-from-and-why-they-are-what-they-are/).

The ``on `` notation allows specification of a timestamp to the predicate, effectively adding it an extra argument. All predicates are "fluents", e.g. considered to have a timestamp, which may be implicit - in which case the predicate holds true "for all time".

The ``P at KP`` metapredicate in a rule body refers a separate source of knowledge:

	someRuleHead if ... and P at "http://someURL" and ...

This allows reference of "external knowledge", e.g. predicates defined in other knowledge pages or external databases. The term *knowledge page* means: the knowledge base fragment encoding the knowledge in web page ``KP``. The truth of ``P`` will therefore be evaluated in KP. This is similar to Prolog's call ```KP:P```, except that ```at``` abstracts from file names and uses a specific (and SWISH-aware) module loading mechanism.

Each predicate P may be:

	* true
	* false
	* unknown

Unknown values can potentially fuel a user dialog. For example, if ``has_aggregated_turnover(tfn1,X) at WWW`` is evaluated as unknown (because the system has no access to the turnover value in order to bind variable X), this could dictate the user question: 

	What is the aggregated turnover of entity tfn1 ? Refer to WWW for more info

Fully bound literals originate yes/no questions. These questions, generated in reaction to a query to the knowledge base, can provide the base for a chatbot. 

###  2.1. <a name='Otherconstructs'></a>Other constructs

In addition to the above features supporting knowledge representation, there are a few more:

	mainGoal(PredicateTemplate, Description).

Each such fact identifies a predicate to be exposed via the (REST) API; however at this point the REST API provides access to any predicate in a knowledge page, so this should be seen as more of a documentation feature.

	example(Title,StatesAndAssertions).
	
This represents a test case, an example of the application of the knowledge rules in the module, providing it with "instance" data specific to some taxpayer, asset, etc. Regulatory (guidance) text sometimes provides them, to lighten their explanations. ``StatesAndAssertions``is an ordered list of ``scenario(Facts, Postcondition)``. Facts is a list of predicate facts (Fact or Fact on Time), with some variants:

 * a prefix ``- `` denotes deletion; the rest of the term must be a fact (nbot a rule). 
 * a prefix ``++`` denotes extension: the fact or rule is added to existing facts and rules (instead of redefining them if no prefix is used); if a single + is used for a predicate, all other example facts and rules for that predicate are also additions (not redefinitions)
 * Head if Body adds a clause; if Body is ``false``, the rule effectively asserts not(Head) as per Negation As Failure
 * Other terms are regarded as facts
 * Head at KP adds the fact (or rule) to the module KP
 

To try an example, for each all facts in the StatesAndAssertions sequence are added/deleted, in sequence, and the PostCondition (assertion) is evaluated at the end.

These example facts are also test cases: all assertions must be true.

**User functions** can be defined locally in a module with the ```function/2``` meta predicate:

	function(project(), Result) if theProject(Result).

This is meant mostly as a convenience/readability feature, e.g. being able to write ```is_exempt( project() )``` instead of ```theProject(Project) and is_exempt(Project)```.

Unknown predicate literals are in fact the "juice" for questions to a human, which can be rendered with more detail if available, via the following optional annotation facts:

	question(Literal,QuestionTerm)		a yes/no question
	question(Literal, QuestionTerm, Answer)	elicit some answer content from the user; the Answer and Literal terms need to share some variable

QuestionTerm can be a string, or a ```FormatString-ArgumentsList``` term, to allow for binding the question string with values, using the [format/2](https://www.swi-prolog.org/pldoc/doc_for?object=format/2) syntax.

###  2.2. <a name='Interfacingtoexternalsystems'></a>Interfacing to external systems

Access to data in external DBs or other services is done via (*unrestricted, possibly with cuts and all*) Prolog code. To have a predicate called directly (instead of it being interpreted like the rest by the Taxlog interpreter), it needs to be declared with a single ```Predicate on Time because Explanation``` clause "stub", allowing the external code both to consider time and to contribute its own bit of explanations; for example:

	is_sme(E) if is_small_business(E).
	
	is_small_business(E) on T because SomeExplanation :-
		call_my_DB(E,T,SomeExplanation).

The ```is_sme``` Taxlog predicate (which is interpreted) calls the ```call_my_DB ``` Prolog predicate directly, sharing time and getting an explanation for the DB result. The provided SomeExplanation term is regarded as an informal predicate literal in the overall explanation tree.

(TODO: fix this hack) SomeExplanation MUST be textually different from '[]'.

##  3. <a name='TowardsLogicalEnglish'></a>Towards Logical English
**"Logical English"** (LE) is an attempt at a design "sweet spot" in the continuum from natural to constrained to formal languages - closer to the latter, but retaining most of the legibility of clear, natural English. For more about it see references at the top of Bob Kowalski's [home page](https://www.doc.ic.ac.uk/~rak/).

Tax-KB includes a preliminary **Logical English generator**, based on introspection of the Taxlog/Prolog clauses and its variable names, dispensing, for the moment, any additional linguistic information. This is intended as a first step towards a future full blown LE module, to include LE parsing; by exposing prospect users to the present LE representantions these can be validated, commented upon, and a precise specification can be established for the future parser. 

From a practical perspective, the LE generator gives an incentive to Taxlog coders to write more readable code, and provides material to share via Word documents etc. with "lawyer-like" people.

###  3.1. <a name='DirectaccesstoLogicalEnglishrendering'></a>Direct access to Logical English rendering

To see it at work, take the module name **KP** of any knowledge page (*the argument in the :-module(KP) directive at its top*), and open the URL "http://demo.logicalcontracts.com:8082/logicalEnglish?kp=**KP**". 

For example, for the KP regarding affiliates in Australian CGT regulations, open:

 > <http://demo.logicalcontracts.com:8082/logicalEnglish?kp=https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/>

Predicates remote to that page have *other legislation* links, which lead to either other "Logical English" pages (if a knowledge page already exists for it) or to the original legislation website. There is always also a link to the Taxlog (executable) representation, in the Tax-KB editor.

The resulting web page can be copied and pasted into Microsoft Word or similar tools.

###  3.2. <a name='ToimprovetheLogicalEnglishrepresentation'></a>To improve the Logical English representation
LE is rendered dynamically from a current knowledge page:

- Open a knowledge page, for example <http://demo.logicalcontracts.com:8082/p/cgt_affiliates.pl>
- Run the goal le(Link). 
- Click the "Show Logical English" button in the Link variable result

If you change your Taxlog rules *and* **save** them, using the File/Save...  menu in the SWISH window, you can then Run ```le(Link)``` again and you'll get access to the updated LE representation. In the above example you might want to try renaming "affiliate" to either "hasAffiliate" or "has_affiliate".

The LE generator has an option to omit indefinite articles, which may arguably lead to more natural sentences: ```le([no_indefinites], Link)```.

###  3.3. <a name='LogicalEnglishstylechecking'></a>Logical English style checking
You may have noticed red curly lines under some sentences, with a tip appearing on hovering the mouse. As "atomic sentences" are generated for each predicate and its arguments, Tax-KB checks (using the SpaCy parser) that they are rooted in a verb. 

Notice that SpaCy, being a neural parser embedding a model trained from many real English sentences, uses the whole sentence to tag words as verbs, nouns etc. Arguably, making SpaCy "happier" (*e.g. making the curly red lines disappear by renaming predicate and argument names*) should reflect into more natural sentences.

###  3.4. <a name='Technicaldetails'></a>Technical details
The Logical English generator is based on cascading two main predicates:

- [le_clause(Head,Module,Ref,LogicalEnglish)](https://github.com/mcalejo/TaxKB/blob/main/le_output.pl#L264), which introspects a single Prolog clause and returns an intermediate LE representation
- [le_kp_html(Module,Options,TermrisedHTML)](https://github.com/mcalejo/TaxKB/blob/main/le_output.pl#L72), which processes the above into HTML. 

The latter includes the (*potentially more expensive*) step of calling the Spacy parser REST service to parse the generated atomic sentences, source code specifically [here](https://github.com/mcalejo/TaxKB/blob/main/le_output.pl#L225).

Standard Prolog clause introspection is used, plus some SWI-Prolog extensions for variable names, combined with CamelCase and under_score [detection](https://github.com/mcalejo/TaxKB/blob/main/drafter.pl#L79)

##  4. <a name='PredicateandwebAPIreference'></a>Predicate and web API reference

###  4.1. <a name='Languageprocessing'></a>Language processing
- ```parseAndSee(Text,SentenceIndex,Tokens,HierplaneTree)``` Useful to explore syntactic and lexical aspects of text
- ```test_draft(Text,DraftedCode)``` given a string with English, try to generate simple Prolog predicate templates for verb phrases
- Web API (POST) for drafting
	- Path: /taxkbapi
	- JSON payload object fields:
		- operation: "draft"
		- pageURL:URLofContent
		- content: ArrayOfItems, each a {url: ChunkURL, text:Text}
	- The result will be a:
		- {pageURL: MyPageURL, draft: StringWithNewPrologModuleSourceCode}
	- Example
		- curl --header "Content-Type: application/json" --request POST --data '{"operation":"draft", "pageURL":"http://mysite/page1#section2",  "content":[{"url":"http://mysite/page1#section2!chunk1", "text":"john flies by instruments"}, {"url":"http://mysite/page1#section2!chunk2", "text":"miguel drives with gusto"}]}' http://demo.logicalcontracts.com:8082/taxkbapi

###  4.2. <a name='PerspectiveonexistingencodedKnowledgePages'></a>Perspective on existing (encoded) Knowledge Pages
*Note: in the following predicates, leaving KP unbound will show not one, but all knowledge pages*
- ```knowledgePagesGraph(KP,Graph)```
	- TIP: hover the top left corner, Download GraphViz Graph, Save/Print to PDF
- ```print_kp_predicates(KP)```
- ```printAllPredicateWords(KP)```
- ```predicateWords(KP,Pred,PredWords)```, ```uniquePredicateSentences(KP,S)```, ```uniqueArgSentences(KP,S)```
- ```le(Link)``` generate Logical English web page and bind Link to a navigation button
	- ```le([no_indefinites],Link)``` same but omitting a/an articles
- Web API (GET) for preliminary Logical English
	- http://demo.logicalcontracts.com:8082/logicalEnglish?kp=**KP**"

###  4.3. <a name='Querying'></a>Querying
- ```query_with_facts(Goal,FactsSource,Unknowns,Explanation,Result)```
	- Goal is of the form ```G at KP```
	- If simply G: KP is assumed to be the module in the current editor
	- If time is relevant use ```G’ on Datetime```
	- FactsSource is either a list of facts/rules or an example name in KP
	- Unknowns are predicate calls assumed true and supporting the answer (we want it to be [])
	- Explanation is a justification of the answer, a tree represented in a large Prolog term which the Tax-KB SWISH renderer displays as an indented list, including navigation links etc.
	- Result is either of true/unknown/false
- Web API (POST) for querying
	- Path: /taxkbapi
	- JSON payload object fields:
		- operation: "query"
		- theQuery:PrologGoalString
		- module: knowledge page name
		- facts: ArrayOfPrologFactStrings (possibly empty list) or name of example in the knowledge page
	- The result will be an array of (for each solution):
		- {result:true/false/unknown, bindings:VarsValues, unknowns:Array, why: Tree}
	- Example
		- curl --header "Content-Type: application/json" --request POST --data '{"operation":"query", "theQuery":"a(13,Y)", "facts":["d(13)"], "module":"http://tests.com"}' http://demo.logicalcontracts.com:8082/taxkbapi
- ```render_questions(Unknown,Questions)``` uses question(…) fact annotations to obtain more readable "questions"

###  4.4. <a name='LEParsing'></a>Parsing Logical English
- ```text_to_logic(LogicalEnglishString, Error, TaxlogTranslation)```
	- LogicalEnglishString is a regular string with with the following structure. The expression:

		the predicates are:

		followed by the declarations of all the predicates involved in the knowledge base.
		Each declarations define a template with the variables and other words required to
		describe a relevant relation. It is a comma separated list of templates which ends
		with a period. 

		After that period, the following statement introduces the knowledge base:

		the knowledge base includes: 

		This is followed by the rules and facts written in Logical English syntax. Each rule must end with a period. 

		Indentation is used to organize the and/or list of conditions by strict
		observance of one condition per line with a level of indentation that 
		correspond to each operator and corresponding conditions. 

		Commentaries can be added with the usual % symbol like in Prolog. 
	- Error contains a report of the last parsed positions inside the file (after removing commentaries) 
		a fragment of the text that contains the error. This variable contains a [] if no error was a found. 
	- TaxlogTranslation contains a list with the terms and predicates obtained from the Logical English text.

- ```le_taxlog_translate(en(LogicalEnglishString), TaxlogTranslation)``` to be used inside the SWISH querying interface
	- LogicalEnglishString as explained above. 
	- TaxlogTranslation as explained above. 

##  5. <a name='Installationanddeployment'></a>Installation and deployment

###  5.1. <a name='Quickrecipeforadevelopmentserver'></a>Quick recipe for a development server
- cd ~ ; git clone https://github.com/mcalejo/TaxKB.git
- docker run -p "127.0.0.1:8080:80" logicalcontracts/spacyapiplus:en_v2
- docker run -it -p 3050:3050 -v \~/TaxKB/swish/data:/data -v \~/TaxKB:/app -e LOAD='/app/swish/user_module_for_swish.pl' -e SPACY_HOST=localhost:8080 -e LOAD_KB=true logicalcontracts/patchedprivateswish 

With your browser go to http://localpost:3050 

###  5.2. <a name='Moredetails'></a>More details
A Tax-KB instance comprises two Docker containers and a copy of this git repository:
- SpacY standalone container, built from this [Dockerfile](https://github.com/mcalejo/TaxKB/blob/main/spacy/docker/Dockerfile) and accessible on port 8080
- SWI-Prolog with (*slightly tweaked, but independent from Tax-KB*) SWISH container, web server included, built from another [Dockerfile](https://github.com/mcalejo/TaxKB/blob/main/swish/dockerfile)
	- This Docker container is launched (*cf. comments at the top of the Dockerfile*) with two host volumes mounted: a SWISH work data directory, and a copy of [this](https://github.com/mcalejo/TaxKB) repository; in addition, several environment variables are passed, referred next

The SWI-Prolog+SWISH Docker container does not contain any Tax-KB specific code. It is "customised" for Tax-KB via parameters passed on startup, as environment variables:
* -e LOAD='/TaxKB/swish/user_module_for_swish.pl' : indicates the startup Prolog file, which loads everything
* -e SPACY_HOST=demo.logicalcontracts.com:8080 : the address of the SpaCy REST service for parsing
* -e SUDO=false: if true (only for development!), this provides a sudo(AnyGoal) Prolog "backdoor meta predicate", shortcircuiting SWISH's safe sandbox restrictions
* -e LOAD_KB=true: whether the SWISH internal storage is loaded at startup with all knowledge pages in the kb/ directory, overwriting any existing ones


###  5.3. <a name='Whereistheknowledge:SWISHstoragevs.filesystem'></a>Where is the knowledge: SWISH storage vs. file system
A word about **SWISH storage**, to understand the need for the last parameter above (LOAD_KB). 

When a user saves a Prolog file with the SWISH web editor, it does not go straightly into the server file system; instead, it is stored in a SWISH internal storage area (persisting in the /data volume mounted in the Docker container); this provides limited versioning, tagging and related services, nicknamed internally as "[gitty storage](https://github.com/SWI-Prolog/swish/blob/master/lib/storage.pl#L83)". Such files can be opened in the SWISH syntax-aware web editor via the URL SERVER/p/filename.pl, and are listed in SWISH's 

But for the sake of easier versioning, integration with the Tax-KB code base etc. the knowledge pages (for the initial 6 samples) are maintained in [kb/](https://github.com/mcalejo/TaxKB/tree/main/kb), not on SWISH storage. So during development, it's convenient that on startup the knowledge pages are copied into SWISH storage, so they appear in SWISH's "file" browser and can be opened easily. Hence the LOAD_KB parameter.

For a deployed system, it may be better to use LOAD_KB=false, so that on restart the Tax-KB preserves the latest user changes to knowldege pages. 

There are two Prolog predicates for copying all knowledge pages between SWISH storage and the kb/ directory:
- load_gitty_files: copies from kb/ into SWISH storage
- save_gitty_files: copies from SWISH storage into the kb/ directory

The latter allows git tracking of server changes, a posteriori. 

When deploying a real system this needs to be tuned, possibly using a specific git branch (e.g. "deployed-version") for the server kb/, so that user changes can later be merged safely into master, after some quality control/review process TBD.

###  5.4 Other details
To update the tokenize/ package: ```git submodule update --init --recursive```

##  6. <a name='Implementationalaspects'></a>Implementational aspects

###  6.1. <a name='Rendering'></a>Rendering
SWISH includes a powerful Prolog term renderer mechanism, allowing the generation of arbitrary HTML (including possibly the reeling in of Javascript components). Tax-KB includes several specific renderers, for:
- [Parse trees](https://github.com/mcalejo/TaxKB/blob/main/spacy/hierplane_renderer.pl); this embeds AllenAI's powerful [hierplane](https://allenai.github.io/hierplane)
	- Token lists are rendered with SWISH's standard [table renderer](http://demo.logicalcontracts.com:8082/example/render_table.swinb)
- Logical English [navigation links](https://github.com/mcalejo/TaxKB/blob/main/swish/le_link_renderer.pl), effectively embedding a hidden form containing the generated HTML with the LE representation
	- this is articulated with the le(..) predicate; for direct URL access to LE this renderer is not used
- [Explanation trees](https://github.com/mcalejo/TaxKB/blob/main/swish/explanation_renderer.pl)
- [Unknowns lists](https://github.com/mcalejo/TaxKB/blob/main/swish/unknowns_renderer.pl)
###  6.2. <a name='Editor'></a>Editor
The SWISH editor was slightly customised:
- A SWISH internal file was patched to provide more flexibility handling "long clicks" (the mouse event for navigation to the selected predicate)
- Taxlog-specific syntax colouring, namely the [taxlog2prolog](https://github.com/mcalejo/TaxKB/blob/main/syntax.pl) predicate.

##  7. <a name='Releases'></a>Release Notes

- [2021-10-12] Alfa testing  Previous version: [ef5a1d66ed4d706829cd9b66a6b1cc052799eac5](https://github.com/mcalejo/TaxKB/commit/ef5a1d66ed4d706829cd9b66a6b1cc052799eac5)
	- The word "that" is no longer allowed as part of variable names, constants or expressions. 
	- Fixing `get_answer_from_goal/2` to include predef templates while producing LE version of an answer. 
- [2021-10-13] Alfa testing.
	- template instances with "that" can be written in more than one line (new line before or after "that")
	- Asteriks in a template must be paired. Otherwise, a syntax error will be issued.
- [2021-10-14] Alfa testing. 
	- Repairing the structure `a variable is a set of a term where .. `. The term is restricted to be another variable, a constant or a list. For example: `[an element, a second element]` 
	- Fixes for `variable`, `term_` and other related predicates. 
- [2021-10-17] Alfa testing.  Previous version: [de95523ab71785c5e46190d0fcea7ad347b689be](https://github.com/mcalejo/TaxKB/commit/de95523ab71785c5e46190d0fcea7ad347b689be)
	- Fixing bug to allow prolog builtins to be used in expressions (op_stop).
	- Adding 014_escrow.pl example
- [2021-10-18] Alfa testing.  Previous version: [bafa1d38c759a9eb0787185ef197276415144144](https://github.com/mcalejo/TaxKB/commit/bafa1d38c759a9eb0787185ef197276415144144) 
	- Fixing bug in translation of `it becomes not the case`. Extending definition of before/2 to support simple numbers (not only dates). Adjusting answer output not to translate any number below 100 to 
	standard format for dates. 
	- Example 014_escrow.pl is fully functional. 
- [2021-10-19] Alfa testing. 
	- Correcting misspelling in 014_escrow.pl
- [2021-10-20] Alfa testing. Previous version: [02010de1abaecb801075454ea5afe06e170356fb](https://github.com/mcalejo/TaxKB/commit/02010de1abaecb801075454ea5afe06e170356fb)
	- Fixing mayor bug in parsing of assumptions in scenarios. 
- [2021-10-21] Alfa testing.
	- Adding "every" to expressions with "the sum of". 
- [2021-10-22] Alfa testing. Previous version: [75dcb4fadb11ac9415d046c1cf15202532ed93f8](https://github.com/mcalejo/TaxKB/commit/75dcb4fadb11ac9415d046c1cf15202532ed93f8)
	- Adding support for Head|Tail list notation in LE. 
- [2021-10-26] Alfa testing. Previous version: [22db8d0a1e80c4254755f869963a8c3a95b9f739](https://github.com/mcalejo/TaxKB/commit/22db8d0a1e80c4254755f869963a8c3a95b9f739).
	- Supressing messages from the translator. 
	- Extending the querying machine to allow direct queries in answer/1. 
	- Adding a second argument to identify the scenario in answer(Query, with(Scenario))
	- Adjusting answer/3 with the same purpose (third argument returns the result)
- [2021-10-28] Alfa testing.
	- Adding commands for querying and showing:
		- `query <query>`
		- `query <query name> with <scenario>`
		- `show prolog`
- [2021-10-29] Alfa testing. [65b8ebb253312c3f4f6f59e2bb41d88d2d49366c](https://github.com/mcalejo/TaxKB/commit/65b8ebb253312c3f4f6f59e2bb41d88d2d49366c)
	- Adding commands for querying and showing:
		- `show templates`: shows the all the available templates (including predef).
		- `show rules`: shows the prolog code corresponding to rules in the kb.
		- `show queries`: shows the prolog code for queries.
		- `show scenarios`: shows the prolo code for scenarios. 
	- Fixing entries in the dictionary (predef).
	- Fixing details in the display of asteriks in templates.  