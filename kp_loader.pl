:- module(_,[
    call_at/2, kp_dir/1,
    discover_kps_in_dir/1, discover_kps_in_dir/0, discover_kps_gitty/0, setup_kp_modules/0,
    load_gitty_files/1, load_gitty_files/0, save_gitty_files/1, delete_gitty_file/1, 
    xref_all/0, xref_clean/0, kp_predicates/0,
    edit_kp/1]).

:- use_module(library(prolog_xref)).
:- use_module(library(broadcast)).
% SWISH MUST be preloaded:
:- use_module(swish(lib/storage)).
:- use_module(swish(lib/gitty)).

:- use_module(syntax).

:- dynamic kp_dir/1.
:- prolog_load_context(directory, D), retractall(kp_dir(_)), atomic_list_concat([D,'/kb'], KD), assert(kp_dir(KD)).


/** <module> Dynamic module loader.

Scans a given set of Prolog files in SWISH storage, and identifies "knowledge pages", files which:

- are modules named with an URL

Can also export and import SWISH storage to/from a file system directory.

%TODO: generalise to use files in a plain file system directory too
*/

:- dynamic kp_location/3. % URL,File, InGitty
kp(URL) :- kp_location(URL,_,_).

%! discover_kps_gitty is det.
%
%  Scans all Prolog files in SWISH's gitty storage for knowledge pages. Reloads
%  loaded modules, but does not delete "orphans" (modules no longer in gitty)
%  TODO: use '$destroy_module'(M) on those?
discover_kps_gitty :-
    retractall(kp_location(_,_,true)),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,_Meta),
        open_string(Data, In),
        process_file(In,File,true)
    )).

%! discover_kps_in_dir(+Dir) is det.
%
discover_kps_in_dir(Dir) :-
    retractall(kp_location(_,_,false)),
    forall(directory_member(Dir,File,[extensions([pl])]), (
        open(File,read,In),
        process_file(In,File,false)
    )).

discover_kps_in_dir :-
    kp_dir(D), discover_kps_in_dir(D).

process_file(In,File,InGitty) :-
    must_be(boolean,InGitty),
    setup_call_cleanup( true, (
        process_terms(In, LastTerm),
        % (LastTerm=at(Name) -> (
        (LastTerm=(:-module(Name,_)) -> (
            assert(kp_location(Name,File,InGitty)),
            % reload the module if it already exists:
            (current_module(Name) -> load_named_file(File,Name,InGitty) ; true)
            ); true)
    ), close(In)).

process_terms(In,Term) :- % actually gets only the first term, where the module declaration must be:
    %repeat, 
    read_term(In, Term, [syntax_errors(fail)]),
    ( Term==end_of_file, ! ; 
        Term= (:- module(URL,_)), is_absolute_url(URL), ! ; 
        true
        %Term=at(Name), (ground(Name)->true; print_message(warning,'ignored'(at(Name))), fail) 
    ).

load_named_file(File,Module,true) :- !,
    use_gitty_file(Module:File,[module(Module)]).
load_named_file(File,Module,false) :- 
    load_files(File,[module(Module)]).

setup_kp_modules :- forall(kp(M), M:discontiguous((if)/2) ).

%! call_at(:Goal,++KnowledgePageName) is nondet.
%
%  Execute goal at the indicated knowledge page, loading it if necessary
call_at(Goal,Name) :- must_be(nonvar,Name), current_module(Name), !, Name:Goal.
call_at(Goal,Name) :- kp_location(Name,File,InGitty), !, 
    load_named_file(File,Name,InGitty),
    Name:Goal.
call_at(Goal,Name) :- print_message(error,'could not load kp'(Goal,Name)), fail.

%! save_gitty_files(+ToDirectory) is det
%
%  ERASES the directory and copies all gitty Prolog files into it
save_gitty_files(_ToDirectory) :- \+ storage_file_extension(_File,pl), !, 
    print_message(warning,"No gitty files to save").
save_gitty_files(ToDirectory) :-
    (exists_directory(ToDirectory)->true; make_directory(ToDirectory)),
    delete_directory_contents(ToDirectory),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,Meta),
        directory_file_path(ToDirectory,File,Path),
        open(Path,write,S), write_term(S,Data,[]), close(S),
        set_time_file(Path, _OldTimes, [modified(Meta.time)])
        )).

%! load_gitty_files(+FromDirectory) is det
%
%  Updates or creates (in gitty storage) all Prolog files from the given file system directory; sub-directories are ignored.
%  Does not delete the other (pre-existing) gitty files
% Example: load_gitty_files('/Users/mc/git/TaxKB/kb').
load_gitty_files(From) :- 
    web_storage:storage_dir(Store),
    forall(directory_member(From,Path,[extensions([pl])]),(
        read_file_to_string(Path,Data,[]),
        directory_file_path(_,File,Path),
        (gitty_file(Store, File, OldHead) -> (
            storage_meta_data(File, Meta), 
            NewMeta = Meta.put([previous=OldHead, modify=[any, login, owner], (public)=true]),
            gitty_update(Store, File, Data, NewMeta, _CommitRet)
            ) ; (
            gitty_create(Store, File, Data, _{load_gitty_files:From, modify:[any, login, owner], public:true }, _CommitRet)
            ))
        )).

load_gitty_files :-
    kp_dir(D), load_gitty_files(D).

%! delete_gitty_file(+GittyFile) is det
%
% makes the file empty, NOT a proper delete
delete_gitty_file(File) :-
    must_be(nonvar,File),
    web_storage:storage_dir(Store),
    gitty_file(Store, File, OldHead),
    % I was unable to effectively delete:
    % gitty:delete_head(Store, OldHead), gitty:delete_object(Store, OldHead), % this is only effective after a SWISH restart
    % broadcast(swish(deleted(File, OldHead))). % not doing anything, possibly missing something on the JS end
    % ... instead this does roughly what the DELETE REST SWISH endpoint in storage.pl does:
    storage_meta_data(File, Meta),
    NewMeta = Meta.put([previous=OldHead]),
    gitty_update(Store, File, "", NewMeta, _CommitRet).



% Support xref for our gitty files
:- multifile
	prolog:xref_source_identifier/2,
	prolog:xref_open_source/2,
    prolog:xref_close_source/2,
    prolog:xref_source_time/2,
    prolog:meta_goal/2.

prolog:xref_source_identifier(URL, URL) :- kp_location(URL,_,_).
prolog:xref_open_source(URL, Stream) :-
    kp_location(URL,File,InGitty),
    (InGitty==true -> (storage_file(File,Data,_Meta), open_string(Data, Stream))
        ; (open(File,read,Stream))).
prolog:xref_close_source(_, Stream) :-
	close(Stream).
prolog:xref_source_time(URL, Modified) :-
    kp_location(URL,File,InGitty),
    (InGitty==true -> (storage_file(File,_,Meta), Modified=Meta.time) ;
        time_file(File,Modified)).


%! xref_all is det
%
% refresh xref database for all knowledge pages %TODO: report syntax errors properly
xref_all :- 
    forall(kp_location(URL,File,_), (print_message(informational,xreferencing/URL-File), xref_source(URL))).

xref_clean :-
    forall(kp_location(URL,_,_), xref_clean(URL)).

:- listen(swish(X),reactToSaved(X)). % note: do NOT use writes!, they would interfere with SWISH's internal REST API

reactToSaved(created(GittyFile,Commit)) :- % discover and xref
    storage_file(GittyFile,Data,_Meta), process_file(Data,GittyFile), 
    reactToSaved(updated(GittyFile,Commit)).
reactToSaved(updated(GittyFile,_Commit)) :- % xref
    kp_location(URL,GittyFile), xref_source(URL).


kp_predicates :- 
    forall(kp(KP),(
        format("KP: ~w~n",[KP]),
        format("  Instance data:~n"),
        forall(xref_defined(KP,G,thread_local(_)), (
            functor(G,F,N), format("    ~w~n",[F/N])
            )),
        format("  Defined predicates:~n"),
        forall((xref_defined(KP,G,How),How\=thread_local(_)), (
            functor(G,F,N), format("    ~w~n",[F/N])
        )),
        format("  External predicates called:~n"),
        forall((xref_called(KP, Called, _By),Called=Other:G,Other\=KP), (
            functor(G,F,N), format("    ~w~n",[F/N])
        ))
    )). 

%! edit_kp(URL) is det
%
% Open the current gitty version of the knowledge page in SWISH's editor
edit_kp(URL) :-
    kp_location(URL,File,InGitty),
    (InGitty==(false) -> print_message(error,"That is not in SWISH storage");(
        format(string(URL),"http://localhost:3050/p/~a",[File]), www_open_url(URL)
        )).


%%%% Knowledge pages graph

:- multifile user:'swish renderer'/2. % to avoid SWISH warnings in other files
:- use_rendering(user:graphviz).

knowledgePagesGraph(dot(digraph([rankdir='LR'|Graph]))) :- 
    % xref_defined(KP, Goal, ?How)
    setof(edge(From->To,[]), KP^Called^By^ByF^ByN^OtherKP^G^CalledF^CalledN^(
        kp(KP), xref_called(KP, Called, By),
        functor(By,ByF,ByN), From = at(ByF/ByN,KP),
        (Called=OtherKP:G -> true ; (OtherKP=KP, G=Called)),
        functor(G,CalledF,CalledN), To = at(CalledF/CalledN,OtherKP) 
        %term_string(From_,From,[quoted(false)]), term_string(To_,To,[quoted(false)]), url_simple(ArcRole_,ArcRole)
        ),Edges), 
    setof(node(ID,[/*shape=Shape*/]), KP^Goal^How^GF^GN^From^EA^(
        (
            kp(KP), xref_defined(KP, Goal, How),
            functor(Goal,GF,GN),
            ID = at(GF/GN,KP)
            ;
            member(edge(From->ID,EA),Edges) % calls to undefined predicates
        )
        %(hypercube(R,ID) -> Shape=box3d ; Shape=ellipse)
        ), Nodes),
    append(Nodes,Edges,Items),
    Graph=Items.
    %(var(SizeInches) -> Graph=Items ; Graph = [size=SizeInches|Items]).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(kp_loader:knowledgePagesGraph(_)).

%%%% assist editor navigation; cf. swish/web/js/codemirror/mode/prolog/prolog_server.js

:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- http_handler(codemirror(xref),   token_references,        []).
token_references(Request) :-
    %http_read_json_dict(Request, Query, [value_string_as(atom)]),
    http_parameters(Request, [arity(Arity,[]),text(Text,[]),type(Type,[]),file(Module,[optional(true)]),uuid(UUID,[optional(true)])]),
    % UUID is the SWISH internal module for our current editor's text
    mylog(gotQuery/Type/Text/Arity/Module/UUID),
    % asserta(my_request(Query)), % for debugging
    (nonvar(UUID) -> (xref_module(UUID,MyModule), Ignorable=[UUID,MyModule]); Ignorable=[]),
    catch(term_string(Term,Text),_,fail), 
    functor(Term,Functor,_),
    (sub_atom(Type, 0, _, _, head) -> ( % a clause head
        must_be(var,Module),
        findall( _{title:Title,line:Line,file:File,target:Functor}, ( % regex built on the Javascript side from target
            xref_called(OtherModule,_Mine:Term,By,_Cond,Line), functor(By,F,N), format(string(Title),"A call to ~a from ~w",[Text,F/N]),
            \+ member(OtherModule,Ignorable),
            kp_location(OtherModule,File,_InGitty) 
            ),Locations)
        ) ; 
        sub_atom(Type, 0, _, _, goal) -> ( % a goal in a clause body
            findall( _{title:Title,line:Line,file:File,target:Functor}, ( 
            xref_defined(Module,Term,How), arg(1,How,Line), format(string(Title),"A definition for ~a",[Text]),
            kp_location(Module,File,_InGitty) 
            ),Locations)
        ) ; 
        throw(weird_token_type(Type))
    ),
    %Solution = _{hello: "Good Afternoon!", functor:Functor, arity:Arity, module:File},
    reply_json_dict(Locations).
