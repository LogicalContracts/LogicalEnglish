:- module(_,[call_at/2, discover_kps_gitty/0, load_gitty_files/1, save_gitty_files/1, delete_gitty_file/1, xref_all/0]).

:- use_module(library(prolog_xref)).
:- use_module(library(broadcast)).
% SWISH MUST be preloaded:
:- use_module(swish(lib/storage)).
:- use_module(swish(lib/gitty)).

:- use_module(syntax).

/** <module> Dynamic module loader.

Scans a given set of Prolog files in SWISH storage, and identifies "knowledge pages", files which:

- are modules named with an URL

Can also export and import SWISH storage to/from a file system directory.

%TODO: generalise to use files in a plain file system directory too
*/

:- dynamic kp_location/2. % URL,GittyFile
% :- dynamic loaded_module/2. % URL,Module.  
kp(URL) :- kp_location(URL,_).

%! discover_kps_gitty is det.
%
%  Scans all Prolog files in SWISH's gitty storage for knowledge pages. Reloads
%  loaded modules, but does not delete "orphans" (modules no longer in gitty)
%  TODO: use '$destroy_module'(M) on those?
discover_kps_gitty :-
    retractall(kp_location(_,_)),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,_Meta),
        process_file(Data,File)
    )).

process_file(Data,File) :-
    setup_call_cleanup( open_string(Data, In), (
        process_terms(In, LastTerm),
        % (LastTerm=at(Name) -> (
        (LastTerm=(:-module(Name,_)) -> (
            assert(kp_location(Name,File)),
            % reload the module if it already exists:
            (current_module(Name) -> load_named_file(File,Name) ; true)
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

load_named_file(File,Module) :- 
    use_gitty_file(Module:File,[module(Module)]).

%! call_at(:Goal,++KnowledgePageName) is nondet.
%
%  Execute goal at the indicated knowledge page, loading it if necessary
call_at(Goal,Name) :- must_be(nonvar,Name), current_module(Name), !, Name:Goal.
call_at(Goal,Name) :- kp_location(Name,File), !, 
    load_named_file(File,Name),
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
load_gitty_files(From) :- 
    web_storage:storage_dir(Store),
    forall(directory_member(From,Path,[extensions([pl])]),(
        read_file_to_string(Path,Data,[]),
        directory_file_path(_,File,Path),
        (gitty_file(Store, File, OldHead) -> (
            storage_meta_data(File, Meta), 
            NewMeta = Meta.put([previous=OldHead]),
            gitty_update(Store, File, Data, NewMeta, _CommitRet)
            ) ; (
            gitty_create(Store, File, Data, _{load_gitty_files:From}, _CommitRet)
            ))
        )).

%! delete_gitty_file(+GittyFile) is det
%
% makes the file empty
delete_gitty_file(File) :-
    must_be(nonvar,File),
    web_storage:storage_dir(Store),
    gitty_file(Store, File, OldHead),
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

prolog:xref_source_identifier(URL, URL) :- kp_location(URL,_).
prolog:xref_open_source(URL, Stream) :-
    kp_location(URL,GittyFile),
    storage_file(GittyFile,Data,_Meta),
	open_string(Data, Stream).
prolog:xref_close_source(_, Stream) :-
	close(Stream).
prolog:xref_source_time(URL, Modified) :-
    kp_location(URL,GittyFile),
    storage_file(GittyFile,_,Meta), Modified=Meta.time.

%TODO: put this near the reasoner? ADD and/or/etc.
prolog:meta_goal(at(G,M),[M_:G]) :- atom_string(M_,M).
prolog:meta_goal(and(A,B),[A,B]).
prolog:meta_goal(or(A,B),[A,B]).
prolog:meta_goal(must(A,B),[A,B]).
prolog:meta_goal(not(A),[A]).
prolog:meta_goal(then(if(C),else(T,Else)),[C,T,Else]).
prolog:meta_goal(then(if(C),Then),[C,Then]) :- Then\=else(_,_).

%! xref_all is det
%
% refresh xref database for all knowledge pages %TODO: report syntax errors properly
xref_all :- 
    forall(kp_location(URL,_), (print_message(informational,xreferencing/URL), xref_source(URL))).

:- listen(swish(X),reactToSaved(X)). % note: do NOT use writes!, they would interfere with SWISH's internal REST API

reactToSaved(created(GittyFile,Commit)) :- % discover and xref
    storage_file(GittyFile,Data,_Meta), process_file(Data,GittyFile), 
    reactToSaved(updated(GittyFile,Commit)).
reactToSaved(updated(GittyFile,_Commit)) :- % xref
    kp_location(URL,GittyFile), xref_source(URL).


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
    (var(SizeInches) -> Graph=Items ; Graph = [size=SizeInches|Items]).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(kp_loader:knowledgePagesGraph(_)).
