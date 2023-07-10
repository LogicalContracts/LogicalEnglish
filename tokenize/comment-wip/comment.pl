:- module(comment,
          [comment//2,
           comment_rec//2,
           comment_token//3,
           comment_token_rec//3]).

/** <module> Tokenizing comments
This module defines matchers for comments used by the tokenize module. (Note
that we will use matcher as a name for dcg rules that match parts of the codes
list).

@author Stefan Israelsson Tampe
@license LGPL v2 or later

Interface Note:
Start and End matchers is a matcher (dcg rule) that is either evaluated with no
extra argument (--> call(StartMatcher)) and it will just match it's token or it
can have an extra argument producing the codes matched by the matcher e.g. used
as --> call(StartMatcher,MatchedCodes). The matchers match start and end codes
of the comment, the 2matcher type will represent these kinds of dcg rules or
matchers 2 is because they support two kinds of arguments to the dcg rules.
For examples
see:

  @see tests/test_comments.pl

The matchers predicates exported and defined are:

 comment(+Start:2matcher,+End:2matcher)
   - anonymously match a non recursive comment

 comment_rec(+Start:2matcher,+End:2matcher,2matcher)
   - anonymously match a recursive comment

 coment_token(+Start:2matcher,+End:2matcher,-Matched:list(codes))
   - match an unrecursive comment outputs the matched sequence used
     for building a resulting comment token

 coment_token_rec(+Start:2matcher,+End:2matcher,-Matched:list(codes))
   - match an recursive comment outputs the matched sequence used
     for building a resulting comment token
*/



%! comment(+Start:2matcher,+End:2matcher)
%    non recursive non tokenizing matcher

comment_body(E) --> call(E),!.
comment_body(E) --> [_],comment_body(E).

comment(S,E) -->
    call(S),
    comment_body(E).

%! comment_token(+Start:2matcher,+End:2matcher,-Matched:list(codes))
%    non recursive tokenizing matcher

comment_body_token(E,Text) -->
    call(E,HE),!,
    {append(HE,[],Text)}.

comment_body_token(E,[X|L]) -->
    [X],
    comment_body_token(E,L).

comment_token(S,E,Text) -->
    call(S,HS),
    {append(HS,T,Text)},
    comment_body_token(E,T).

%! comment_token_rec(+Start:2matcher,+End:2matcher,-Matched:list(codes))
%   recursive tokenizing matcher

% Use this as the initial continuation, will just tidy up the matched result
% by ending the list with [].
comment_body_rec_start(_,_,[]).

comment_body_token_rec(_,E,Cont,Text) -->
    call(E,HE),!,
    {append(HE,T,Text)},
    call(Cont,T).

comment_body_token_rec(S,E,Cont,Text) -->
    call(S,HS),!,
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_token_rec(S,E,Cont),T).

comment_body_token_rec(S,E,Cont,[X|L]) -->
    [X],
    comment_body_token_rec(S,E,Cont,L).

comment_token_rec(S,E,Text) -->
    call(S,HS),
    {append(HS,T,Text)},
    comment_body_token_rec(S,E,comment_body_rec_start,T).

%! comment_rec(+Start:2matcher,+End:2matcher)
%    recursive non tokenizing matcher

comment_body_rec(_,E) -->
    call(E),!.

comment_body_rec(S,E) -->
    call(S),!,
    comment_body_rec(S,E),
    comment_body_rec(S,E).

comment_body_rec(S,E) -->
    [_],
    comment_body_rec(S,E).

comment_rec(S,E) -->
    call(S),
    comment_body_rec(S,E).
