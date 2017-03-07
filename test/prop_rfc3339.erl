-module(prop_rfc3339).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

prop_format_parse_date() ->
    ?FORALL(Date, rfc3339:date(),
            ?IMPLIES(calendar:valid_date(Date),
                     rfc3339:parse_date(rfc3339:format_date(Date)) =:= Date)).

prop_format_parse_time() ->
    ?FORALL(Time, rfc3339:time(),
            rfc3339:parse_time(rfc3339:format_time(Time)) =:= Time).

prop_format_parse_datetime() ->
    ?FORALL({Date, Time, Frac},
            {rfc3339:date(), rfc3339:time(), rfc3339:fraction_unit()},
            ?IMPLIES(calendar:valid_date(Date),
                     rfc3339:parse_datetime(
                       rfc3339:format_datetime({Date, Time}, Frac)) =:=
                         {{Date, Time}, Frac})).

prop_format_parse_local_datetime() ->
    ?FORALL({Date, Time, Offset, Frac},
            {rfc3339:date(), rfc3339:time(), rfc3339:offset(), rfc3339:fraction_unit()},
            ?IMPLIES(calendar:valid_date(Date),
                     rfc3339:parse_local_datetime(
                       rfc3339:format_local_datetime({Date, Time}, Offset, Frac)) =:=
                         {{Date, Time}, Offset, Frac})).
