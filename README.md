# TaxKB
Development of Prolog based generic knowledge base for tax regulations, including reasoner, editor and other tools

## Initial sample of examples

The following tax regulation fragments were indicated:

### LodgeIT

Provided AN email Nov 28, 2020; these are in addition to the already prototyped <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/> :

- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/small-business-restructure-rollover/>
	- Bob's GoogleDoc: <https://docs.google.com/document/d/1fj8Cjp_FNKAIXvrUYD52Zpfgo6Ftqbypd8s36mC7CdA/edit?ts=5fc7e58b>
- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/maximum-net-asset-value-test/>
	- Bob's GoogleDoc: <https://docs.google.com/document/d/1wJl_JzZ7tkMMyia3eKdz1XlX6JBhYIcngJTESZQjz6E/edit?ts=5fd3aafc&skip_itp2_check=true>
	- TODO: encode this also from the LEGISLATION
- <https://www.ato.gov.au/general/capital-gains-tax/small-business-cgt-concessions/basic-conditions-for-the-small-business-cgt-concessions/affiliates/>
	- Further references (ignored in the first encoding): <https://pointonpartners.com.au/small-business-cgt-passing-the-threshold-tests/>

This was indicated (Dec 10 AN email) for later analysis: <https://www.taxtalks.com.au/small-business-participation-percentage/>


### AORALaw
_Some suggestions for simplification and more info in first and second Chris emails, Dec 2, 2020_

- <https://www.gov.uk/guidance/stamp-duty-reserve-tax-reliefs-and-exemptions>
- <https://www.gov.uk/hmrc-internal-manuals/residence-domicile-and-remittance-basis/rdrm11040>
	- Bob's GoogleDoc: <https://docs.google.com/document/d/1nzoGuzPKI375jxtxT38jCEYwuhuZvjSBSHmvxusW0Lg/edit?ts=5fc92d43#heading=h.pzozuu8bp8r0>
- <https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief>

## The Tax-KB language

This is the language used to encode knowledge gleaned from regulatory textual sources. The text therein comprises 3 types of fragments: 

- comments (to be effectively ignored); 
- rule ingredients (the main thing)
- examples

The term *knowledge page* denotes a Prolog module that encodes the knowledge in some web (or accessible via URL) page containing the regulatory text. This knowledge excludes instance (specific case) data, to be added in runtime. It is declared with a standard Prolog module declaration:

	:- module('http://myURLBase',[]).

This means that the knowldege page was constructed from the text at that URL. It must be the very first Prolog term in the file.

The logic language used is pure PROLOG (Horn clauses) but sugared with some additional operators in the clause bodies, towards readability and expressiveness:

  * ``if``, ``and``, ``or``, as alternatives to :- , ;
  * ``if...must/then..``  and ``if...then...else...``
  * Predicate ``on `` Moment
  * Predicate ``at`` KnowledgePageURL

The "deontic" ``if Condition (it) must (be true that) Obligation`` in a clause body is simply mapped into ``Condition and Obligation or not Condition``. The term "deontic" is being stretched here, cf. for example this explanation on [deonthic vs. alethic](https://www.brsolutions.com/the-two-fundamental-kinds-of-business-rules-where-they-come-from-and-why-they-are-what-they-are/).

The ``on `` notation allows specification of a timestamp to the predicate, effectively adding it an extra argument. By default all predicate truth values report to "now", a datetime typically associated with the main time focus of the knowledge page.

The ``P at KP`` metapredicate has several meanings, depending on where it is used. 

If a rule for some predicate ``P`` has a more specific provenance than the whole module's (perhaps a sub-section or text span identified by a more specific URL, possibly with an anchor), this can be encoded using ``at`` in its head:

	P at "#MySubsection" if ....
	
The URL must be relative, and will be concatenated to the myURLBase above. Finally, there is ``at``in a rule body:

	someRuleHead if ... and P at "http://someURL" and ...

This allows reference of "external knowledge", e.g. predicates defined in other knowledge pages or external databases. The term *knowledge page* means: the knowledge base fragment encoding the knowledge in web page ``KP``. The truth of ``P`` will therefore be evaluated in KP. This is similar to Prolog's call ```KP:P```, except that ```at``` abstracts from file names and uses a specific module loading mechanism.

Each predicate P may be:

	* true
	* false
	* unknown

Unknown values can fuel a user dialog. For example, if ``has_aggregated_turnover(tfn1,X) at WWW`` is evaluated as unknown (because the system has no access to the turnover value in order to bind variable X), this dictates the user question: 

	What is the aggregated turnover of entity tfn1 ? Refer to WWW for more info

Fully bound literals originate yes/no questions. These questions, generated in reaction to a query to the knowledge base, can provide the base for a chatbot. 

### Other constructs

In addition to the above features supporting knowledge representation, there are a few more:

	mainGoal(PredicateTemplate, Description).

Each such fact identifies a predicate to be exposed via the (REST) API.

	example(Title,StatesAndAssertions).
	
This represents an example of the application of the knowledge rules in the module, providing it with "instance" data specific to some taxpayer, asset, etc. Regulatory (guidance) text sometimes provides them, to lighten their explanations. ``StatesAndAssertions``is an ordered list of ``state(Facts, Postcondition)``. Facts is a list of predicate facts, each optionally prefixed with ``- `` for deletion. To try an example, for each all facts in the StatesAndAssertions sequence are added/deleted, and the PostCondition (assertion) is evaluated.

These example facts are also test cases: all assertions must be true.

**User functions** can be defined locally in a module with the ```function/2``` meta predicate:

	function(project(), Result) if theProject(Result).

This is meant mostly as a convenience/readability feature, e.g. being able to write ```is_exempt( project() )``` instead of ```theProject(Project) and is_exempt(Project)```.

**TODO**: document contexts where functions can be used

Some predicates are so hard to specify that they may actually require a question to a human:

	question(QuestionTerm)		a yes/no question
	question(QuestionTerm,Answer)	elicit an answer from the user

QuestionTerm can be a string, or a ```FormatString-ArgumentsList``` term, to allow for binding the question string with values, using the [format/2](https://www.swi-prolog.org/pldoc/doc_for?object=format/2) syntax.