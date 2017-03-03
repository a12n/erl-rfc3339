-module(prop_rfc3339).

-compile(export_all).

-include_lib("proper/include/proper.hrl").

prop_format_parse_date() ->
    ?FORALL(Date, rfc3339:date(),
            ?IMPLIES(calendar:valid_date(Date),
                     rfc3339:parse_date(rfc3339:format_date(Date)) =:= Date)).

prop_format_parse_time() ->
    ?FORALL(Time, rfc3339:time(),
            ?IMPLIES(valid_time(Time),
                     rfc3339:parse_time(rfc3339:format_time(Time)) =:= Time)).

prop_format_parse_datetime() ->
    ?FORALL({Date, Time, {Frac, Unit}},
            {rfc3339:date(), rfc3339:time(),
             oneof([{integer(0, 999), millisecond},
                    {integer(0, 999999), microsecond},
                    {integer(0, 999999999), nanosecond}])},
            ?IMPLIES(calendar:valid_date(Date) andalso valid_time(Time),
                     rfc3339:parse_datetime(
                       rfc3339:format_datetime({Date, Time}, Frac, Unit)) =:=
                         {{Date, Time}, Frac, Unit})).

prop_format_parse_local_datetime() ->
    ?FORALL({Date, Time, Offset, {Frac, Unit}},
            {rfc3339:date(), rfc3339:time(), rfc3339:offset(),
             oneof([{integer(0, 999), millisecond},
                    {integer(0, 999999), microsecond},
                    {integer(0, 999999999), nanosecond}])},
            ?IMPLIES(calendar:valid_date(Date) andalso valid_time(Time),
                     rfc3339:parse_local_datetime(
                       rfc3339:format_local_datetime({Date, Time}, Offset, Frac, Unit)) =:=
                         {{Date, Time}, Offset, Frac, Unit})).

%%--------------------------------------------------------------------

valid_time({Hour, Minute, Second})
  when Hour >= 0, Hour =< 23,
       Minute >= 0, Minute =< 59,
       Second >= 0, Second =< 59;
       Hour =:= 23, Minute =:= 23, Second =:= 60 ->
    true;

valid_time(_Other) -> false.
