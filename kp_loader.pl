:- module(_,[call_at/2, discover_kps_gitty/0, load_gitty_files/1, save_gitty_files/1]).

% SWISH MUST be preloaded:
:- use_module(swish(lib/storage)).
:- use_module(swish(lib/gitty)).

/** <module> Dynamic module loader.

Scans a given set of Prolog files in SWISH storage, and identifies "knowledge pages", files which:

- have an at(Name) fact. Name is typically an URL
- do NOT have a module declaration.

Can also export and import SWISH storage to/from a file system directory.

*/

:- dynamic kp_location/2. % URL,GittyFile
:- dynamic loaded_module/2. % URL,Module.  

%! discover_kps_gitty is det.
%
%  Scans all Prolog files in SWISH's gitty storage for knowledge pages. Reloads
%  loaded modules, but does not delete "orphans" (modules no longer in gitty)
discover_kps_gitty :-
    retractall(kp_location(_,_)),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,_Meta),
        process_file(Data,File)
    )).

process_file(Data,File) :-
    setup_call_cleanup( open_string(Data, In), (
        process_terms(In, LastTerm),
        (LastTerm=at(Name) -> (
            assert(kp_location(Name,File)),
            % reload the module if it already exists:
            (retract(loaded_module(Name,M)) -> load_named_file(File,Name,M) ; true)
            ); true)
    ), close(In)).

process_terms(In,Term) :-
    repeat, 
    read_term(In, Term, [syntax_errors(fail)]),
    ( Term==end_of_file, ! ; 
        Term= (:- module(_)), ! ; % ignore it
        Term=at(Name), (ground(Name)->true; print_message(warning,'ignored'(at(Name))), fail) 
    ).

load_named_file(File,Name,Module) :- 
    (var(Module) -> uuid(Module); true),
    use_gitty_file(Module:File,[module(Module)]), assert(loaded_module(Name,Module)).

%! call_at(:Goal,++KnowledgePageName) is nondet.
%
%  Execute goal at the indicated knowledge page, loading it if necessary
call_at(Goal,Name) :- must_be(nonvar,Name), loaded_module(Name,M), !, M:Goal.
call_at(Goal,Name) :- kp_location(Name,File), !, 
    uuid(M), load_named_file(File,Name,M),
    M:Goal.
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
%  Updates or creates (in gitty storage) all Prolog files from the given directory; sub-directories are ignored.
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