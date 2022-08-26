# Latest (New LE EDITOR mode for VSC):

We are happy to announce that we are inviting for testing of the new LE editor mode for the Visual Studio Code Editor, VSC, developed by Nikolai Merritt, as his MSc project at Imperial College. The whole code is here:

[https://github.com/nikolaimerritt/LogicalEnglish](https://github.com/nikolaimerritt/LogicalEnglish)

But the VSC extension can be obtained directly from within VSC. As the author explains: "Installing the extension is now very simple. Having installed Visual Studio Code, from the 'extensions' icon on the left bar, and search for "logical-english-vscode". (The extension comes up if you search for "logical english" or similar, but gets buried under lots of less new extensions.) It can also be found here [https://marketplace.visualstudio.com/items?itemName=NikolaiMerritt.logical-english-vscode](https://marketplace.visualstudio.com/items?itemName=NikolaiMerritt.logical-english-vscode)"

We want to encourage our friends to test it and, if you are so kind, report the experiences here:

[https://docs.google.com/forms/d/e/1FAIpQLSeYH3D_6Lc8F0DB7Z1lvqmRaqBgYYFqxr6NtFAgx1Ffa510Bw/viewform](https://docs.google.com/forms/d/e/1FAIpQLSeYH3D_6Lc8F0DB7Z1lvqmRaqBgYYFqxr6NtFAgx1Ffa510Bw/viewform)

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

## Projects using LE

### [Accziom.com](https://accziom.com/logical-english/)

Introduction and list of resources at [Advent of Logical English​​](https://accziom.com/advent-of-logical-english/)

### [University of Bologna](https://legalmachinelab.unibo.it/logicalenglish/)


## Development RoadMap

1. *TESTING NOW* **Improve the error detection system** for better pinpointing of error location and cause.
2. *TESTING NOW* a. **Store LE translation as a prolog or a s(CASP) file** to be injected into SWISH storage (to appear in the SWISH file list).
3. **Add a facility to name rules**, initially to refer to them in the explainer, but eventually to incorporate naming of rules with rule priorities, to deal with hierarchies of rules and exceptions. Note that the resulting implementation of defeasibility can be accomplished either by means of a meta-interpreter, or by compiling rule priorities into lower-level Prolog rules with explicit negative conditions.
4. *TESTING NOW* **Add a facility for naming and combining knowledge bases**, with a view to obtaining some of the functionality of object-oriented systems, including inheritance and overriding.
5. b. **Develop an editor for LE** with syntax and semantic highlighting, and error detection, as a separate editor project, preferably on JS Codemirror, using simple pengine calls to a SWISH server to assist the editing, and convert to different formats. 
6. c. AFTER (b) is done, add .le as a valid SWISH file type.
7. **Add a treatment of common nouns as types**.
8. Update older examples into the new syntax extensions. 

## Support RoadMap

1. **Opening the discussion forum**
2. **Update** the LE handbook with the latest syntax extensions.
3. Add more examples (both to the report and the handbook). 

## Older Examples under review

1. [1_net_asset_value_test](./kb/1_net_asset_value_test.pl) Determine if a given entity satisfies the maximum net value test for Capital Gain Test, CGT, assets. 
2. [2_r_and_d_tax_reliefs](./kb/2_r_and_d_tax_reliefs.pl) Determine if a project qualifies for the EIS. 
3. [3_rollover_2](./kb/3_rollover_2.pl) Decide whether the small business restructure rollover applies to an event.
4. [4_affiliates](./kb/4_affiliates.pl) Determine if a given entity is affiliate of another (also given). 
5. [5_tax_reliefs](./kb/5_tax_reliefs.pl) Determine if an electronic transaction is exempt from SDRT. 
6. [6_statutory_residence](./kb/6_statutory_residence.pl) Determine if a person is a UK resident for tax purposes. 
7. [7_loan_agreement](./kb/7_loan_agreement.pl) The Loan Agreement (and many variations with and without the Event Calculus)
8. [014_escrow](./kb/014_escrow.pl) A blockchain-like escrow agreement. 
9. [cittadinanza_ita](./kb/cittadinanza_ita.pl) Cittadinanza Italiana (Italian Citizenship, many versions and translation into sCASP).
10. [010_isda_agreement](./kb/010_isda_agreement.pl) The ISDA Agreement (Many versions including [isda-permission-corrected](./kb/isda-permission-corrected.pl))
11. [oecd_test_for_inclusion](./kb/oecd_test_for_inclusion.pl) OECD example with an included document. 
12. [criminal_justice](./kb/criminal_justice.pl) Criminal justice example. 
13. [subsets](./kb/subsets.pl) Subsets example in many languages and into Prolog and sCASP. 
14. [permitted that](./kb/permitted%20that.pl) Example with metavariables. 
15. [minicontractv2](./kb/minicontractv2.pl) A proposed format for an electronic contract. 


##  <a name='Releases'></a>Release Notes

- [2022-05-12] Beta testing. Previous version [97abce7d4398f05c1f165e4a411a982b16f358bd](https://github.com/LogicalContracts/LogicalEnglish/commit/97abce7d4398f05c1f165e4a411a982b16f358bd)
	- Activates #abducible to handle unknown information with sCASP in LE. 
- [2022-05-05] Beta testing. Previous version: [d405af4da4fbeb170cd1cb1be333ff66c65a8d98](https://github.com/LogicalContracts/LogicalEnglish/commit/d405af4da4fbeb170cd1cb1be333ff66c65a8d98)
	- Merging branch logical_languages with the latest extensions of LE syntax for sources in Italian, French and Spanish and targeting sCASP beside Prolog. Also, inclusion of other documents. 
- [2021-11-23] Updating the roadmap. Previous version: [c4d67e9e0dbc54356473b284c9b72725c6504673](https://github.com/mcalejo/TaxKB/commit/c4d67e9e0dbc54356473b284c9b72725c6504673)
- [2021-11-08] Beta testing. 
	- Adding extract_variable_template/7 as specific predicates for templates. Fixing bug in output of answers with dates. 
- [2021-11-07] Beta testing. Previous version: [92d90631853812384463c1cb2c2f1607e0a3fd64](https://github.com/mcalejo/TaxKB/commit/92d90631853812384463c1cb2c2f1607e0a3fd64)
	- Adding the command show types, to display the list of type identifier available in predefined templates and in the current document. 
	- Templates are therefore formally used to capture type information for variables. Any combination of words between the asteriks identifies a type, except for ordinal. 
	- Added is_type/1 (dynamic predicate) and pre_is_type/1 (for predefined templates) to record that information in memory. 
	- Correcting the use of dictionary/3 in le_output
- [2021-11-02] Alfa testing. Last version: [eb0039077d068dd718ef47fd81af95554faeaef3](https://github.com/mcalejo/TaxKB/commit/eb0039077d068dd718ef47fd81af95554faeaef3)
	- Updates to documentation, README.md, licensing and querying machinery.
- [2022-02-06] Update of the server. Last version: [6b89eb9e75066d1074769005e9675ffa2b4d7470](https://github.com/LogicalContracts/LogicalEnglish/commit/6b89eb9e75066d1074769005e9675ffa2b4d7470)
	- Explanation facilities improved and operational via `answer(query, with(scenario), le(E), R)`
	- Fixed multi-level indentation. 
	- Parsing speed improved.
	- Translation from LE to s(CASP) via `show(scasp, with(query, scenario))`.
	- Changes for error reporting inside conditions.
	- New examples. 
	



