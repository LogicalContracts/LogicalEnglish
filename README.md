# TaxLog and Logical English projects - PRELIMINARY

For more about TaxLog see [TaxLog.md](TaxLog.md) file.

For a general Introduction to Logical English, LE, (in PDF): [Handbook](le_handbook.pdf). 

See this [video](https://vimeo.com/643589682) introducing Logical English

For a more formal description of LE: [le_syntax.md](le_syntax.md)

For more about Logical English visit [the testing site at LogicalContracts.com](https://logicalenglish.logicalcontracts.com/example/LogicalEnglish.swinb) and [the paper "Logical English for Legal Applications"](http://www.doc.ic.ac.uk/~rak/papers/LE_for_LA.pdf)

Software shared "as is", no warranties nor support whatsoever. 

## Licensing and copyright

All software in this directory is licensed as per Apache License 2.0 (https://www.apache.org/licenses/LICENSE-2.0) except where noted.

Initial copyright holders by country: LodgeIT (AU), AORA Law (UK), Bob Kowalski (UK), Miguel Calejo (PT), Jacinto Dávila (VE)

Special thanks to: Andrew Noble, Chris Mennell and Bruce Mennell


## RoadMap

1. **Improve the error detection system** for better pinpointing of error location and cause.
2. **Develop and improve the existing LE explanation facility**, with a view to using it as an alternative to the Prolog debugger.
3. **Add a facility to name rules**, initially to refer to them in the explainer, but eventually to incorporate naming of rules with rule priorities, to deal with hierarchies of rules and exceptions. Note that the resulting implementation of defeasibility can be accomplished either by means of a meta-interpreter, or by compiling rule priorities into lower-level Prolog rules with explicit negative conditions.
4. **Add a facility for naming and combining knowledge bases**, with a view to obtaining some of the functionality of object-oriented systems, including inheritance and overriding.
5. **Add a treatment of common nouns as types**.
6. **Develop an editor for LE** with syntax and semantic highlighting, and error detection.

##  <a name='Releases'></a>Release Notes

- [2021-11-23] Updating the roadmap. Previous version: [c4d67e9e0dbc54356473b284c9b72725c6504673](https://github.com/mcalejo/TaxKB/commit/c4d67e9e0dbc54356473b284c9b72725c6504673)
- [2021-11-08] Beta testing. 
	- Adding extract_variable_template/7 as specific predicates for templates. Fixing bug in output of answers with dates. 
- [2021-11-07] Beta testing. Previous version: [92d90631853812384463c1cb2c2f1607e0a3fd64](https://github.com/mcalejo/TaxKB/commit/92d90631853812384463c1cb2c2f1607e0a3fd64)
	- Adding the command show types, to display the list of type identifier available in predefined templates and in the current document. 
	- Templates are therefore formally used to capture type informmation for variables. Any combination of words between the asteriks identifies a type, except for ordinal. 
	- Added is_type/1 (dynamic predicate) and pre_is_type/1 (for predefined templates) to record that information in memory. 
	- Correcting the use of dictionary/3 in le_output
- [2021-11-02] Alfa testing. Last version: [eb0039077d068dd718ef47fd81af95554faeaef3](https://github.com/mcalejo/TaxKB/commit/eb0039077d068dd718ef47fd81af95554faeaef3)
	- Updates to documentation, README.md, licensing and querying machinery.

