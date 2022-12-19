% our loader, with a peculair name prefix to make sure it loads after other config files

:- print_message(informational,"Using local config files for SWISH"-[]).
:- prolog_load_context(directory, D), 
    (sub_atom(D,0,_,_,'/data') -> 
        print_message(informational,"Assuming Docker execution"),
        ['/app/swish/user_module_for_swish.pl']
        ;        
        print_message(informational,"Assuming execution in developer machine"),
        ['../user_module_for_swish.pl'] 
    ).
