% Written by Miguel Calejo for LodgeIT, Australia, and AORALaw, UK; copyright LodgeIT+AORALaw (50% each) 2020
% PRELIMINARY DRAFT

% For now, residing in the 'user' module

% Web page from which the present knowledge page was encoded
myURL("https://www.gov.uk/guidance/corporation-tax-research-and-development-rd-relief").

% Assumptions: 
%   all predicates hold on NOW unlesss indicated otherwise with 'on'; 
%   datetimes in iso_8601 format
%   external predicates MUST be aware of the local main event time, "now"

:- use_module(syntax).
:- discontiguous (if)/2.

% project_expense(TaxPayer,ProjectID,Cost,When)
% project_team(ProjectID,MemberList)
% project_subject_experts(ProjectID,ExpertsList)
project() := P if 
    project_expense(_TaxPayer,P,_Cost,_When).

r_d_relief(ExtraDeduction,TaxCredit) if
    qualify_for_r_d and ( % specific qualifying conditions are to be encoded in these:
        sme_r_d_relief(ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-small-and-medium-sized-enterprises" 
        or 
        r_d_expense_credit(ExtraDeduction,TaxCredit) at "https://www.gov.uk/guidance/corporation-tax-research-and-development-tax-relief-for-large-companies"
    ).

qualify_for_r_d if
    aims_advance_in_field and
    professionals_could_not_doit and
    there_was_uncertainty and
    overcame_uncertainty.

aims_advance_in_field if 
    question( "Does the project aim to create an advance in the overall field, not just for your business?
        This means an advance cannot just be an existing technology that has been used for the first time in your sector.
        The process, product or service can still be an advance if it’s been developed by another company but is not publicly known or available" ).

professionals_could_not_doit if
    question("Show how other attempts to find a solution had failed",_Why).
professionals_could_not_doit if
    project_team(project(),Members) and Member in Members 
    and question("~w, explaing the uncertainty involved"-Member,_Explanation).

there_was_uncertainty if
    project_subject_experts(project(),Experts) and Expert in Experts
    and question("~w, is it true that you could not know about the project advances or how they were going to be accomplished ?").

overcame_uncertainty if
    question( "Explain the work done to overcome the uncertainty. This can be a simple description of the successes and failures you had during the project.
    Show that the R&D needed research, testing and analysis", _Explanation).