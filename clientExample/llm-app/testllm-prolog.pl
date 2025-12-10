:-module('testllm-prolog', []).
source_lang(en).
local_dict([the_thermal_load_through_may_be, A, B], [enclosure-enclosure, value-value], [the, thermal, load, through, A, may, be, B]).
local_dict([the_thermal_load_through_is_minimal, A], [enclosure-enclosure], [the, thermal, load, through, A, is, minimal]).
local_dict([high_energy_efficiency_is_achieved_within, A], [enclosure-enclosure], [high, energy, efficiency, is, achieved, within, A]).
local_dict([the_windows_of_are, A, B], [enclosure-enclosure, state-state], [the, windows, of, A, are, B]).
local_dict([the_temperature_in_is_correct, A], [enclosure-enclosure], [the, temperature, in, A, is, correct]).
local_dict([the_radiation_inside_is_correct, A], [enclosure-enclosure], [the, radiation, inside, A, is, correct]).
local_dict([the_air_conditioning_is, A], [state-state], [the, air, conditioning, is, A]).
local_dict([humidity_within_is_correct, A], [enclosure-enclosure], [humidity, within, A, is, correct]).
local_dict([is_less_than, A, B], [amount-amount, other_amount-other_amount], [A, is, less, than, B]).
local_dict([is_comfortable_in, A, B], [group-group, enclosure-enclosure], [A, is, comfortable, in, B]).
local_dict([the_wind_blows], [], [the, wind, blows]).
local_dict([has_affiliated_with_at, _, A, B], [entity-entity, affiliate-affiliate, date-date], [who, affiliated, with, A, at, B]).
local_dict([has_affiliated_with_at, _, A, B], [entity-entity, affiliate-affiliate, date-date], [what, entity, affiliated, with, A, at, B]).
local_dict([has_affiliated_with_at, A, B, _], [entity-entity, affiliate-affiliate, date-date], [when, did, A, affiliate, with, B]).
local_dict([has_affiliated_with_at, A, B, _], [entity-entity, affiliate-affiliate, date-date], [on, what, date, did, A, affiliate, with, B]).
local_dict([has_affiliated_with_at, A, B, _], [entity-entity, affiliate-affiliate, date-date], [is, there, an, affiliation, between, A, and, B]).
local_dict([has_affiliated_with_at, A, B, _], [entity-entity, affiliate-affiliate, date-date], [when, did, the, affiliation, between, A, and, B, begin]).
local_dict([is_of_type, A, B], [object-object, type-type], [A, is, of, type, B]).
local_dict([is_a, A, B], [object-object, type-type], [A, is, an, B]).
local_dict([is_a, A, B], [object-object, type-type], [A, is, a, B]).
local_dict([is_a, A, B], [object-object, type-type], [A, is, of, B]).
local_dict([has_as_head_before, A, B, C], [list-list, symbol-term, rest_of_list-list], [A, has, B, as, head, before, C]).
local_dict([same_date, A, B], [time_t1-time, time_t2-time], [A, is, the, same, date, as, B]).
local_dict([is_1_day_after, A, B], [date_one-date, date_two-date], [A, is, '1', day, after, B]).
local_dict([is_days_after, A, B, C], [date_one-date, number-number, date_two-date], [A, is, B, days, after, C]).
local_dict([immediately_before, A, B], [time_t1-time, time_t2-time], [A, is, immediately, before, B]).
local_dict([is_not_before, A, B], [time_t1-time, time_t2-time], [A, is, not, before, B]).
local_dict([isbeforeorequal, A, B], [time_t1-date, time_t2-date], [A, is, before, or, equal, to, B]).
local_dict([isafterorequal, A, B], [time_t1-date, time_t2-date], [A, is, after, or, equal, to, B]).
local_dict([isbefore, A, B], [time_t1-date, time_t2-date], [A, is, before, B]).
local_dict([isafter, A, B], [time_t1-date, time_t2-date], [A, is, after, B]).
local_dict([unparse_time, A, B], [secs-time, date-date], [A, corresponds, to, date, B]).
local_dict([length, A, B], [member-object, list-list], [the, length, of, A, is, B]).
local_dict([bagof, A, B, C], [bag-thing, thing-thing, condition-condition], [C, is, a, bag, of, A, such, that, B]).
local_dict([append, A, B, C], [first_list-list, second_list-list, third_list-list], [appending, A, then, B, gives, C]).
local_dict([reverse, A, B], [list-list, other_list-list], [A, is, the, reverse, of, B]).
local_dict([between, A, B, C], [min-date, max-date, middle-date], [C, is, between, A, &, B]).
local_dict([\=, A, B], [entity_X-thing, entity_Y-thing], [A, is, different, from, B]).
local_dict([==, A, B], [entity_X-thing, entity_Y-thing], [A, is, equivalent, to, B]).
local_dict([=, A, B], [entity_X-thing, entity_Y-thing], [A, is, equal, to, B]).
local_dict([=<, A, B], [entity_X-thing, entity_Y-thing], [A, is, less, or, equal, to, B]).
local_dict([<, A, B], [entity_X-thing, entity_Y-thing], [A, is, less, than, B]).
local_dict([>=, A, B], [entity_X-thing, entity_Y-thing], [A, is, greater, or, equal, to, B]).
local_dict([>, A, B], [entity_X-thing, entity_Y-thing], [A, is, greater, than, B]).
local_dict([member, A, B], [member-object, list-list], [A, is, in, B]).
local_dict([nonvar, A], [entity_X-thing], [A, is, known]).
local_dict([=, A, B], [entity_X-thing, entity_Y-thing], [A, is, B]).
local_dict([\=@=, A, B], [entity_X-thing, entity_Y-thing], [A, \, =, @, =, B]).
local_dict([\==, A, B], [entity_X-thing, entity_Y-thing], [A, \, =, =, B]).
local_dict([=\=, A, B], [entity_X-thing, entity_Y-thing], [A, =, \, =, B]).
local_dict([=@=, A, B], [entity_X-thing, entity_Y-thing], [A, =, @, =, B]).
local_dict([==, A, B], [entity_X-thing, entity_Y-thing], [A, =, =, B]).
local_dict([=<, A, B], [entity_X-thing, entity_Y-thing], [A, =, <, B]).
local_dict([>=, A, B], [entity_X-thing, entity_Y-thing], [A, >, =, B]).
local_dict([is, A, B], [entity_X-thing, entity_Y-thing], [A, =, B]).
local_dict([<, A, B], [entity_X-thing, entity_Y-thing], [A, <, B]).
local_dict([>, A, B], [entity_X-thing, entity_Y-thing], [A, >, B]).
local_meta_dict([\=, A, B], [first_thing-time, second_thing-time], [A, is, different, from, B]).
local_meta_dict([=, A, B], [first_thing-time, second_thing-time], [A, is, equal, to, B]).
local_meta_dict([nonvar, A], [entity_X-thing], [A, is, known]).
prolog_le(verified).
is_less_than(low, medium).
is_less_than(low, high).
is_less_than(medium, high).
is_comfortable_in(_, A) :-
    the_radiation_inside_is_correct(A),
    the_temperature_in_is_correct(A),
    humidity_within_is_correct(A).
high_energy_efficiency_is_achieved_within(A) :-
    the_thermal_load_through_is_minimal(A).
the_thermal_load_through_is_minimal(A) :-
    the_thermal_load_through_may_be(A, B),
    not(( the_thermal_load_through_may_be(A, C),
          is_less_than(C, B)
        )).
the_thermal_load_through_may_be(_, high) :-
    the_air_conditioning_is(on).
the_thermal_load_through_may_be(A, medium) :-
    the_wind_blows,
    the_windows_of_are(A, open).
the_thermal_load_through_may_be(A, low) :-
    the_air_conditioning_is(off),
    the_wind_blows,
    the_windows_of_are(A, open).
example(null, []).
example(test, [scenario([(the_wind_blows:-true), (the_windows_of_are('my house', open):-true), (the_air_conditioning_is(off):-true), (the_radiation_inside_is_correct('my house'):-true), (the_temperature_in_is_correct('my house'):-true), (humidity_within_is_correct('my house'):-true)], true)]).
example(new, [scenario([(the_wind_blows:-true), (the_windows_of_are('my house', open):-true), (the_air_conditioning_is(on):-true), (the_radiation_inside_is_correct('my house'):-true), (the_temperature_in_is_correct('my house'):-true), (humidity_within_is_correct('my house'):-true)], true)]).
query(null, true).
query(one, is_comfortable_in('my family', _)).
query(two, high_energy_efficiency_is_achieved_within(_)).
query(three, (is_comfortable_in('my family', A), high_energy_efficiency_is_achieved_within(A))).
query(new, (is_comfortable_in('my family', A), high_energy_efficiency_is_achieved_within(A))).
