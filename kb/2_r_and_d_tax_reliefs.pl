:-module('one+https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief',[]).

mainGoal('can_request_R_&_D_relief_for_with'(_ProjectID,_ExtraDeduction,_TaxCredit), "Determine if a project qualifies for the EIS").

example('email Chris Feb 17 - 3A',[
    scenario([
        has_its_members_in(ID,[adam,brad,claire]),
        has(ID,[diana,elaine,fred]),
        looked_for_in_the_field(ID, advance),
        '\'_s_previous_attempts_have_failed'(ID),
        explained_uncertainties_as(_AnyExpert,"unlikely that it could be done"),
        tried_to_overcome_uncertainty_by(ID,_) if false
        ], not 'qualifies_for_research_&_development_relief'(ID))
    ]) :- ID = wirelessFridge.

example('email Chris Feb 17 - 3B',[
    scenario([
        has_its_members_in(ID,[brown]),
        looked_for_in_the_field(ID, advance),
        '\'_s_previous_attempts_have_failed'(ID),
        explained_uncertainties_as(_AnyExpert,"it might not levitate"),
        had_to_overcome_uncertainty(ID), % override rule below, as we don't know the specific experts
        tried_to_overcome_uncertainty_by(ID,"it worked well!")
        ], 'qualifies_for_research_&_development_relief'(ID))
    ]) :- ID = hoverboard.

example('email Chris Feb 17 - 3C',[
    scenario([
        has_its_members_in(ID,[george,hillary]),
        '\'_s_previous_attempts_have_failed'(ID),
        had_to_overcome_uncertainty(ID), % override rule below, as we don't know the specific experts
        tried_to_overcome_uncertainty_by(ID,"it bound properly!")
        ], not 'qualifies_for_research_&_development_relief'(ID))
    ]) :- ID = candyfloss.

en("the templates are:
    a project can request R&D relief for a credit with a deduction,
    a project qualifies for research & development relief,
    the SME R&D relief for a project is estimated at a first amount with a second amount according to other legislation,
    a project 's R&D expense credit is equal to a first amount plus a second amount according to other legislation,
    a project qualifies for research & development relief,
    a project looked for an advance in the field,
    a project could not be worked out by a professional in the field,
    a project had to overcome uncertainty,
    a project tried to overcome uncertainty by a solution,
    a project's previous attempts have failed,
    a project has its members in a list,
    a member is in the members list,
    a member explained uncertainties as a reason,
    a project has an experts list, 
    an expert is in an experts list,
    a project could not be explained by an expert.


the knowledge base includes:
A project can request R&D relief for a credit with a deduction
    if the project qualifies for research & development relief
    and the SME R&D relief for the project is estimated at the credit with the deduction according to other legislation
        or the project 's R&D expense credit is equal to the credit plus the deduction according to other legislation
. 

A project qualifies for research & development relief 
    if the project looked for an advance in the field
    and the project could not be worked out by a professional in the field
    and the project had to overcome uncertainty
    and the project tried to overcome uncertainty by a solution.

A project could not be worked out by a professional in the field 
    if the project's previous attempts have failed.

A project could not be worked out by a professional in the field 
    if the project has its members in a list
    and a member is in the list
    and the member explained uncertainties as a reason.
 
A project had to overcome uncertainty 
    if the project has an experts list 
    and an expert is in the experts list
    and the project could not be explained by the expert.
    "). 

% the actual questions are really about... rendering the unknowns, so we'll put it all together instead:
% question(UnknownLiteral, QuestionTemplate) or question(UnknownLiteral, QuestionTemplate, AnswerPlaceholder)
question( looked_for_in_the_field(ID, advance), "Does the project ~w aim to create an advance in the overall field, not just for your business?
 This means an advance cannot just be an existing technology that has been used for the first time in your sector.
 The process, product or service can still be an advance if it’s been developed by another company but is not publicly known or available" - ID ).
question( '\'_s_previous_attempts_have_failed'(Project),"Show how other attempts to find a solution for ~w had failed"-Project).
question( explained_uncertainties_as(Member,Explanation), "~w, explaing the uncertainty involved"-Member,Explanation).
question( could_not_be_explained_by(Project,Expert) , "~w, is it true that you could not know about the project ~w advances or how they were going to be accomplished ?"-[Expert,Project]).
question( tried_to_overcome_uncertainty_by(Project,How), "Explain the work done in ~w to overcome the uncertainty. This can be a simple description of the successes and failures you had during the project.
        Show that the R&D needed research, testing and analysis"-Project, How).

/** <examples>
?- query_with_facts('qualifies_for_research_&_development_relief'(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3A',Unknowns,Explanation,Result).
?- query_with_facts('qualifies_for_research_&_development_relief'(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3B',Unknowns,Explanation,Result).
?- query_with_facts('qualifies_for_research_&_development_relief'(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3C',Unknowns,Explanation,Result), render_questions(Unknowns,Q).
*/