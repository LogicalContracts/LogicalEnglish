# The Syntax of Logical English, LE

Version 1.0

## Logical English, LE, in brief

The parser of LE assumes a document with the following structure. One declaration title like:

    the templates are:

followed by the declarations of all the corresponding predicates mentioned in the
knowledge base.

Each declarations define a template with the variables and other words required to
describe a relevant relation. It is a comma separated list of templates which ends
with a period.

After that period, one of the following statement introduces the knowledge base:

    the knowledge base includes:
    the knowledge base <Name> includes:

And it is followed by the rules and facts written in Logical English syntax.

Each rule must end with a period. The exact form is described in the following sections. 

Indentation is used to organize the and/or list of conditions by strict observance of one condition per line with a level of indentation that corresponds to each operator and corresponding conditions.

Similarly, there may be sections for scenarios and queries, like:

    scenario test is:
        borrower pays an amount to lender on 2015-06-01.

and

    query one is:
        for which event:
            the small business restructure rollover applies to the event.

    query two is:
        which tax payer is a party of which event.

    query three is:
        A first time is after a second time
        and the second time is immediately before the first time.


 which can then be used on the new command interface of LE on SWISH
(e.g. answer/1 and others querying predicates):

? answer("query one with scenario test"). 

## Formal description

A document is a declaration section followed by a content section.

A declaration section is a collection of some or all of the following:
meta templates (headed by "the meta predicates are:"),
fluents predicates (headed by "the fluents are:"),  
event predicates  (headed by "the event predicates are:"),
and other predicates (headed by "the templates are:"),
separated by full stops or commas.
A declaration may also include a statement about the target language.

### Declarations

The target language (to translate from LE into) is either taxlog or prolog (taxlog is the default). The actual language declaration starts with the expression "the target language is" and ends with a full stop.
A template is a string of words and  variable indicators separated by spaces.
A word is string of letters or numbers.
A variable indicator is a string of words prefixed by * and postfixed by *.

An instance of a template is an expression obtained from the template by consistently replacing all the variable indicators by terms.

### Content

A name is a string of words separated by spaces.
A variable is a name which is not a proper name, i.e. it is not associated to a particular object or relationship.
A fact is an instance of a template that ends with a full stop.
A rule is an instance of a template followed by the word "if", followed by either one condition or many conditions separated by logical connectives "and" or "or", followed by a full stop.
Rules are sensitive to text indentation. Indentation is defined as the number of spaces between an end of line and the beginning of a condition.      

Each condition is allocated its own separated line, preceded by indentation and the corresponding operator, if any. e.g. (the rectangles indicate the indentation of each condition):

        and the animal is a bird
        and the bird can fly
            or the bird is a penguin  

Operators have a level of indentation that corresponds to their precedence. Therefore, different logical connectives are constrained to have different levels of indentation, as shown in the example above.

A condition
A condition is
an instance of a template (the system provides some predefined templates including Prolog's binary predicates)
Or a variable followed by the expression "is a collection of", followed by an instance of template, followed by a new line, an indentation and the word "where" just before a list of conditions. i.e.

    *a variable* is a collection of *an instance of a template*
                where *a list of conditions*

Or an expression that starts with "for all cases in which" followed by a new line, an indentation and a list of conditions, which will followed by one or more ends of line and the expression "it is the case that", followed in turn by a new linea, another indentation and another list of conditions.

Or a variable, followed by the expression "is the sum of each", followed by a variable, followed by the expression "such that", followed by a new line, an indentation and a list of conditions, in that order.
Or the expression "it is not the case that", followed in order by a new line, an indentation and a list of conditions.
Or a condition is a term, followed by one of prolog's binary, built-in predicates, followed by another term.
A term is either a variable, an expression (which include constants) or a compound (such as a list).

### Query

a query is an expression starting with "query", followed in order by a name, the word  "is", a ":", a new line, an indentation, a list of query conditions separated by "and" or "or", and a full stop.
a query condition is 
a condition as before
or a condition in which a variable is prefixed with the word "which" instead of "a" or "an".

For example, the query condition:

    which person loves which person

is asking for a person who loves him/herself. To ask the more general question in which the persons involved are different, an explicit name distinction is required. For instance:

        which person loves which other person
or
        which person X loves which person Y.

### Scenario

a scenario is is an expression starting with "scenario", followed in order by a name, the word  "is", a ":", a new line, an indentation, a list of assumptions separated by a full stop. Assumptions are facts or rules, as described above. 


### Types

As said before, a variable is a name, i.e. a string of words. Some of those words may identify types such as "time", "date", "person", "number" or "day".
A type word may be omitted from the name of a variable after the variable has been introduced. This allow for the use of symbolic variables, like in this expression:

    a number X is greater than a number Y if
        X > Y.

However, type words will not be omitted when they are the only word in the name, e.g.:

    a person is richer than an other person if
        the person has more wealth than the other person.

### "that" word

The word "that" is used in instances of templates to signal a meta-variable.
A meta-variable is a variable that can be replaced by a instance of a template. For example:

    it is permitted that a comment is added to a document.

when the templates are:

    it is permitted that *an event*,
    *a comment* is added to *a document*.

this approach to meta-variable only works when "that" and the meta variable appear last in the sentence.
The corresponding templates with "that" should be declared before the other templates.  


## Additional experimental features


A meta template is a template with at least one variable indicator that can be instantiated by a template.
A fluent template is a templates that represents a state that may change.
An event template is a template that represents an action or event that may change the fluents.


### Meta templates    

The word "that" permits that any template and corresponding template instance include others template instances so nested at any depth. However, it is limited in that "that" only appears once in each sentence.

Declaring meta templates is a more general approach that allows more than one meta-variable per sentences. For example:
the meta predicates are:

        *an event* initiates *a fluent* at *a time*

the predicates are:

        switching the light *a position*,
        darkness.

which would allow rules or facts such as:

        switching the light off initiates darkness at each time.


