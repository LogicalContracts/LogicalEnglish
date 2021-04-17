% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
:-module('https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief',[]).

mainGoal(r_d_relief(_ProjectID,_ExtraDeduction,_TaxCredit), "Determine if a project qualifies for the EIS").

example('email Chris Feb 17 - 3A',[
    scenario([
        project_team(ID,[adam,brad,claire]),
        project_subject_experts(ID,[diana,elaine,fred]),
        aims_advance_in_field(ID),
        other_attempts_have_failed(ID),
        uncertainties_involved_explained_by(_AnyExpert,"unlikely that it could be done"),
        overcame_uncertainty(ID,_) if false
        ], not qualify_for_r_d(ID))
    ]) :- ID = wirelessFridge.

example('email Chris Feb 17 - 3B',[
    scenario([
        project_team(ID,[brown]),
        aims_advance_in_field(ID),
        other_attempts_have_failed(ID),
        uncertainties_involved_explained_by(_AnyExpert,"it might not levitate"),
        there_was_uncertainty(ID), % override rule below, as we don't know the specific experts
        overcame_uncertainty(ID,"it worked well!")
        ], qualify_for_r_d(ID))
    ]) :- ID = hoverboard.

example('email Chris Feb 17 - 3C',[
    scenario([
        project_team(ID,[george,hillary]),
        other_attempts_have_failed(ID),
        there_was_uncertainty(ID), % override rule below, as we don't know the specific experts
        overcame_uncertainty(ID,"it bound properly!")
        ], not qualify_for_r_d(ID))
    ]) :- ID = candyfloss.


/* doesn't seem very useful, using extra arguments for project further below:
:- thread_local theProject/1.
function(project(), Result) if 
    theProject(Result).
*/

:- thread_local project_team/2. % ProjectID, MemberList
:- thread_local project_subject_experts/2. % ProjectID, ExpertsList

% Assumptions: 
%   all predicates hold on FOREVER unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format

r_d_relief(ProjectID,ExtraDeduction,TaxCredit) if
    qualify_for_r_d(project()) and ( % specific qualifying conditions are to be encoded in these:
        sme_r_d_relief(ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-small-and-medium-sized-enterprises" 
        or 
        r_d_expense_credit(ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-large-companies"
    ).

qualify_for_r_d(P) if
    aims_advance_in_field(P) and
    professionals_could_not_doit(P) and
    there_was_uncertainty(P) and
    overcame_uncertainty(P,_How).


professionals_could_not_doit(P) if
    other_attempts_have_failed(P).

professionals_could_not_doit(P) if
    project_team(P,Members) and Member in Members 
    and uncertainties_involved_explained_by(Member,_What).

there_was_uncertainty(P) if
    project_subject_experts(P,Experts) and Expert in Experts
    and could_not_know_about_advances_or_how(P,Expert).

% the actual questions are really about... rendering the unknowns, so we'll put it all together instead:
% question(UnknownLiteral, QuestionTemplate) or question(UnknownLiteral, QuestionTemplate, AnswerPlaceholder)
question( aims_advance_in_field(ID), "Does the project ~w aim to create an advance in the overall field, not just for your business?
 This means an advance cannot just be an existing technology that has been used for the first time in your sector.
 The process, product or service can still be an advance if it’s been developed by another company but is not publicly known or available" - ID ).
question( other_attempts_have_failed(P,How),"Show how other attempts to find a solution for ~w had failed"-P,How).
question( uncertainties_involved_explained_by(Member,Explanation), "~w, explaing the uncertainty involved"-Member,Explanation).
question( could_not_know_about_advances_or_how(P,Expert) , "~w, is it true that you could not know about the project ~w advances or how they were going to be accomplished ?"-[Expert,P]).
question( overcame_uncertainty(P,How), "Explain the work done in ~w to overcome the uncertainty. This can be a simple description of the successes and failures you had during the project.
        Show that the R&D needed research, testing and analysis"-P, How).

/** <examples>
?- query_with_facts(qualify_for_r_d(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3A',Unknowns,Explanation,Result).
?- query_with_facts(qualify_for_r_d(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3B',Unknowns,Explanation,Result).
?- query_with_facts(qualify_for_r_d(Project) at 'https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief','email Chris Feb 17 - 3C',Unknowns,Explanation,Result), render_questions(Unknowns,Q).
?- le(LogicalEnglish).
*/
