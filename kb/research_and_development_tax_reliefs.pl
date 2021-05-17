% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief',[]).

mainGoal('can_request_R&D_relief_such_as'(_ProjectID,_ExtraDeduction,_TaxCredit), "Determine if a project qualifies for the EIS").

example('email Chris Feb 17 - 3A',[
    scenario([
        '\'s_list_of_members_is'(ID,[adam,brad,claire]),
        project_subject_experts_list_is(ID,[diana,elaine,fred]),
        looked_for_an_advance_in_the_field(ID),
        '\'s_previous_attempts_have_failed'(ID),
        explained_uncertainties_as(_AnyExpert,"unlikely that it could be done"),
        tried_to_overcome_uncertainty_by(ID,_) if false
        ], not qualifies_for_a_research_and_development_relief(ID))
    ]) :- ID = wirelessFridge.

example('email Chris Feb 17 - 3B',[
    scenario([
        '\'s_list_of_members_is'(ID,[brown]),
        looked_for_an_advance_in_the_field(ID),
        '\'s_previous_attempts_have_failed'(ID),
        explained_uncertainties_as(_AnyExpert,"it might not levitate"),
        had_to_overcome_uncertainty(ID), % override rule below, as we don't know the specific experts
        tried_to_overcome_uncertainty_by(ID,"it worked well!")
        ], qualifies_for_a_research_and_development_relief(ID))
    ]) :- ID = hoverboard.

example('email Chris Feb 17 - 3C',[
    scenario([
        '\'s_list_of_members_is'(ID,[george,hillary]),
        '\'s_previous_attempts_have_failed'(ID),
        had_to_overcome_uncertainty(ID), % override rule below, as we don't know the specific experts
        tried_to_overcome_uncertainty_by(ID,"it bound properly!")
        ], not qualifies_for_a_research_and_development_relief(ID))
    ]) :- ID = candyfloss.


/* doesn't seem very useful, using extra arguments for project further below:
:- thread_local theProject/1.
function(project(), Result) if 
    theProject(Result).
*/

:- thread_local '\'s_list_of_members_is'/2. % ProjectID, MemberList
:- thread_local project_subject_experts_list_is/2. % ProjectID, ExpertsList

% Assumptions: 
%   all predicates hold on FOREVER unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format

'can_request_R&D_relief_such_as'(Project,ExtraDeduction,TaxCredit) if
    qualifies_for_a_research_and_development_relief(Project) and ( % specific qualifying conditions are to be encoded in these:
        '\'s_sme_R&D_relief_is'(Project,ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-small-and-medium-sized-enterprises" 
        or 
        '\'s_R&D_expense_credit_is'(Project,ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-large-companies"
    ). 

qualifies_for_a_research_and_development_relief(Project) if
    looked_for_an_advance_in_the_field(Project) and
    could_not_be_worked_out_by_a_professional_in_the_field(Project) and
    had_to_overcome_uncertainty(Project) and
    tried_to_overcome_uncertainty_by(Project,_Solution).

could_not_be_worked_out_by_a_professional_in_the_field(Project) if
    '\'s_previous_attempts_have_failed'(Project).

could_not_be_worked_out_by_a_professional_in_the_field(Project) if
    '\'s_list_of_members_is'(Project,MembersList) and in(Member, MembersList) 
    and explained_uncertainties_as(Member,_Reason).

had_to_overcome_uncertainty(Project) if
    project_subject_experts_list_is(Project,Experts) and in(Expert, Experts)
    and could_not_be_explained_or_anticipated_by(Project,Expert).

% the actual questions are really about... rendering the unknowns, so we'll put it all together instead:
% question(UnknownLiteral, QuestionTemplate) or question(UnknownLiteral, QuestionTemplate, AnswerPlaceholder)
question( looked_for_an_advance_in_the_field(ID), "Does the project ~w aim to create an advance in the overall field, not just for your business?
 This means an advance cannot just be an existing technology that has been used for the first time in your sector.
 The process, product or service can still be an advance if it’s been developed by another company but is not publicly known or available" - ID ).
question( '\'s_previous_attempts_have_failed'(Project,How),"Show how other attempts to find a solution for ~w had failed"-Project,How).
question( explained_uncertainties_as(Member,Explanation), "~w, explaing the uncertainty involved"-Member,Explanation).
question( could_not_be_explained_or_anticipated_by(Project,Expert) , "~w, is it true that you could not know about the project ~w advances or how they were going to be accomplished ?"-[Expert,Project]).
question( tried_to_overcome_uncertainty_by(Project,How), "Explain the work done in ~w to overcome the uncertainty. This can be a simple description of the successes and failures you had during the project.
        Show that the R&D needed research, testing and analysis"-Project, How).

/** <examples>
?- query_with_facts(qualifies_for_a_research_and_development_relief(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3A',Unknowns,Explanation,Result).
?- query_with_facts(qualifies_for_a_research_and_development_relief(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3B',Unknowns,Explanation,Result).
?- query_with_facts(qualifies_for_a_research_and_development_relief(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3C',Unknowns,Explanation,Result), render_questions(Unknowns,Q).
?- le(LogicalEnglish).
*/
