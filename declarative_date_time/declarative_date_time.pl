:- module(declarative_date_time, [
  valid_date/1, % valid_date_smt/1,
  to_date/2,
  is_duration_before/3
]).

:- use_module(library(clpfd)).
:- use_module(library(date_time)).

% :- use_module(library(janus)).
% :- py_add_lib_dir(declarative_date_time).
% :- py_add_lib_dir(.).

valid_year(Year) :- Year in 1900..2200.

is_duration_before(Date, Duration, Date) :-
  member(D, [days, weeks, months, years]),
  Duration =.. [D, 0],
  ( Date = today ;
    % valid_date_z3(Date)
    (
      valid_date(Date),
      Date =.. [date | Year_month_day],
      label(Year_month_day)
    )
  ).

is_duration_before(Date0, Duration, Date1) :-
  maplist(to_date, [Date0, Date1], [Date0_, Date1_]),
  is_duration_before_dates(Date0_, Duration, Date1_).

to_date(Date, Date_) :-
  member(Date, [yesterday, today, tomorrow]),
  date_get(Date, Date_).

to_date(Date, Date) :- valid_date(Date).

is_duration_before_dates(Date0, Duration, Date1) :-
  Date0 = date(Year0, Month0, Day0),
  Date1 = date(Year1, Month1, Day1),

  Duration =.. [Duration_f, Duration_num],
  member(Duration_f, [days, weeks, months, years]),
  Duration_num #>= 0,

  maplist(valid_date, [Date0, Date1]),
  lex_chain([[Year0, Month0, Day0], [Year1, Month1, Day1]]),

  ( maplist(integer, [Duration_num, Day0, Month0, Year0]), !,
    date_add(Date0, Duration, Date1)
    ;
    maplist(integer, [Duration_num, Day1, Month1, Year1]), !,
    Duration_neg =.. [Duration_f, -Duration_num],
    date_add(Date1, Duration_neg, Date0)
    ;
    label([Year0, Year1, Month0, Month1, Day0, Day1]),
    % valid_date_pair_z3(Date0, Date1),
    % writeln([Date0, Date1]),
    date_interval(Date1, Date0, Duration)
  ).

valid_date(date(Year, Month, Day)) :-
  valid_year(Year),
  Month in 1..12,
  Day in 1..31,
  (Month in 4 \/ 6 \/ 9 \/ 11) #==> Day #=< 30,
  Month #= 2 #==> Day #=< 29,
  (Month #= 2 #/\ Day #= 29) #==> ((Year mod 400 #= 0) #\/ (Year mod 4 #= 0 #/\ Year mod 100 #\= 0)).

% valid_date_smt(date(Year, Month, Day)) :-
%   (integer(Day) -> Day_in = Day ; Day_in = day),
%   (integer(Month) -> Month_in = Month ; Month_in = month),
%   (integer(Year) -> Year_in = Year ; Year_in = year),
%   py_iter(date_time_smt:valid_date_smt(Day_in, Month_in, Year_in), [Day, Month, Year]).

% valid_date_pair_smt(date(Year0, Month0, Day0), date(Year1, Month1, Day1)) :-
%   (integer(Day0) -> Day0_in = Day0 ; Day0_in = day),
%   (integer(Month0) -> Month0_in = Month0 ; Month0_in = month),
%   (integer(Year0) -> Year0_in = Year0 ; Year0_in = year),
%   (integer(Day1) -> Day1_in = Day1 ; Day1_in = day),
%   (integer(Month1) -> Month1_in = Month1 ; Month1_in = month),
%   (integer(Year1) -> Year1_in = Year1 ; Year1_in = year),
%   py_iter(
%     date_time_smt:valid_date_pair_smt(Day0_in, Month0_in, Year0_in, Day1_in, Month1_in, Year1_in),
%     [Day0, Month0, Year0, Day1, Month1, Year1]
%   ).