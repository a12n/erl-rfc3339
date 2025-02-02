-module(rfc3339).

%% Types
-export_type([year/0, month/0, day/0, date/0,
              hour/0, minute/0, second/0, time/0,
              fraction_unit/0, fraction/0,
              datetime/0, offset/0,
              error/0]).

%% API
-export([format_datetime/1, format_datetime/2,
         format_local_datetime/2, format_local_datetime/3,
         format_date/1,
         format_time/1]).

%% API
-export([parse_datetime/1,
         parse_local_datetime/1,
         parse_date/1,
         parse_time/1]).

-ifndef(pre18).
%% API
-export([format_system_time/1, format_system_time/2,
         parse_system_time/1, parse_system_time/2]).
-endif.

-define(IS_DIGITS(A), A >= $0, A =< $9).
-define(IS_DIGITS(A, B), ?IS_DIGITS(A), ?IS_DIGITS(B)).
-define(IS_DIGITS(A, B, C, D), ?IS_DIGITS(A, B), ?IS_DIGITS(C, D)).

-compile({inline, [digits_to_integer/1, digits_to_integer/2, digits_to_integer/4]}).

%%%===================================================================
%%% Types
%%%===================================================================

-type year() :: 0..9999.
-type month() :: 1..12.
-type day() :: 1..31.

-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.

-type fraction_unit() ::
        {0..999, millisecond}
      | {0..999999, microsecond}
      | {0..999999999, nanosecond}.
-type fraction() :: 0..999999 | fraction_unit().

-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.
-type datetime() :: {date(), time()}.

-type offset() :: {-23..23, minute()}.

-type error() :: badarg | baddate | badtime | badfrac | badoffset.

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv format_local_datetime(DateTime, _Offset = {0, 0})
-spec format_datetime(datetime()) -> iodata().

format_datetime(DateTime) ->
    format_local_datetime(DateTime, _Offset = {0, 0}).

%%--------------------------------------------------------------------

%% @equiv format_local_datetime(DateTime, _Offset = {0, 0}, Frac)
-spec format_datetime(datetime(), fraction()) -> iodata().

format_datetime(DateTime, Frac) ->
    format_local_datetime(DateTime, _Offset = {0, 0}, Frac).

%%--------------------------------------------------------------------

%% @doc
%% Format timestamp with UTC offset. Function behaviour is undefined
%% for invalid input values.
%% @end
-spec format_local_datetime(datetime(), offset() | undefined) -> iodata().

format_local_datetime(_DateTime = {Date, Time}, Offset) ->
    [format_date(Date), $T, format_time(Time), format_offset(Offset)].

%%--------------------------------------------------------------------

%% @doc
%% Format timestamp with UTC offset and fraction of a second. Function
%% behaviour is undefined for invalid input values.
%% @end
-spec format_local_datetime(datetime(), offset() | undefined, fraction()) -> iodata().

format_local_datetime(_DateTime = {Date, Time}, Offset, Frac) ->
    [format_date(Date), $T, format_time(Time, Frac), format_offset(Offset)].

%%--------------------------------------------------------------------

%% @doc
%% Format date. E.g., `iolist_to_binary({@module}:format_date({2016,
%% 6, 20}))' will result in `<<"2016-06-20">>'. Year value must be
%% less than 10000. Function behaviour is undefined for invalid input.
%% @end
-spec format_date(date()) -> iodata().

format_date(_Date = {Year, Month, Day}) ->
    [format4(Year), $-, format2(Month), $-, format2(Day)].

%%--------------------------------------------------------------------

%% @doc
%% Format time of day. E.g.,
%% `iolist_to_binary({@module}:format_time({9, 38, 14}))' will result
%% in `<<"09:38:14">>' binary string. Function behaviour is undefined
%% for invalid input values.
%% @end
-spec format_time(time()) -> iodata().

format_time(_Time = {Hour, Minute, Second}) ->
    [format2(Hour), $:, format2(Minute), $:, format2(Second)].

%%--------------------------------------------------------------------

-spec format_time(time(), fraction()) -> iodata().

format_time(Time, Frac) ->
    [format_time(Time), $., format_fraction(Frac)].

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Parse timestamp and convert it to UTC. Parsing with this function
%% is equivalent to parsing with parse_local_datetime/1 and then
%% compensating for UTC offset.
%%
%% For timestamps with unknown UTC offset (offset is "-00:00" in the
%% source text, parsed as `undefined') conversion isn't possible and
%% `badoffset' atom will be thrown.
%% @end
%% @throws error()
%% @see parse_local_datetime/1
-spec parse_datetime(iodata()) -> {datetime(), fraction() | undefined}.

parse_datetime(Str) ->
    case parse_local_datetime(Str) of
        {_Local, _Offset = undefined, _Frac} -> throw(badoffset);
        {Local, Offset, Frac} -> {remove_offset(Local, Offset), Frac}
    end.

%%--------------------------------------------------------------------

%% @doc
%% Parse timestamp with UTC offset.
%%
%% Date part of the timestamp must represent a valid date, or
%% `baddate' atom will be thrown.
%%
%% Symbols "T", "t" and " " (space) are allowed as separators between
%% date and time of day parts of the timestamp.
%%
%% For invalid time of day value atom `badtime' will be thrown. Leap
%% seconds are allowed in the time of day part, along with any valid
%% hour and minute and will be mapped to second 59. According to UTC,
%% leap seconds are allowed only at end of month, but it seems
%% impractical to implement such a check, and leap second at any hour
%% and minute will be parsed and remapped to second 59.
%%
%% Fraction of a second is parsed with minimal fraction unit
%% possible. E.g., fraction "52" will be parsed as `{520,
%% millisecond}', fraction "5234" will be parsed as `{523400,
%% microsecond}'. Sub-nanosecond fractions aren't supported for now
%% and atom `badfrac' will be thrown. If timestamp doesn't have
%% fraction of a second in the source text, fraction will be parsed as
%% atom `undefined'.
%%
%% UTC offsets from "-23:59" to "+23:59" all considered to be valid and
%% will be parsed. UTC offset "Z" (or "z") is parsed as zero offset
%% `{0, 0}'. Unknown UTC offset ("-00:00" in the source text) is parsed
%% as atom `undefined'.
%% @end
%% @throws error()
-spec parse_local_datetime(iodata()) ->
                                  {datetime(), offset() | undefined,
                                   fraction() | undefined}.

parse_local_datetime(Str) when is_binary(Str) ->
    case parse_date_part(Str) of
        {Date, <<Sep, TimeStr/bytes>>}
          when Sep =:= $T;
               Sep =:= $t;
               Sep =:= $\s ->
            case parse_time_part(TimeStr) of
                {Time, <<$., FracStr/bytes>>} ->
                    case parse_frac_part(FracStr) of
                        {Frac, OffsetStr} ->
                            case parse_offset_part(OffsetStr) of
                                {Offset, <<>>} -> {{Date, Time}, Offset, Frac};
                                {_Offset, _StrLeft} -> throw(badarg)
                            end
                    end;
                {Time, OffsetStr} ->
                    Frac = undefined,
                    case parse_offset_part(OffsetStr) of
                        {Offset, <<>>} -> {{Date, Time}, Offset, Frac};
                        {_Offset, _StrLeft} -> throw(badarg)
                    end
            end;
        {_Date, _BadStr} -> throw(badarg)
    end;

parse_local_datetime(Str) when is_list(Str) ->
    parse_local_datetime(iolist_to_binary(Str)).

%%--------------------------------------------------------------------

%% @doc
%% Parse date. E.g., `{@module}:parse_date(<<"2016-06-20">>)' will
%% result in `{2016, 6, 20}' term. Function throws atom `badarg' for
%% syntactically incorrect input and throws `baddate' when invalid
%% date value parsed.
%% @end
%% @throws badarg | baddate
-spec parse_date(iodata()) -> date().

parse_date(Str) when is_binary(Str) ->
    case parse_date_part(Str) of
        {Date, <<>>} -> Date;
        {_Date, _StrLeft} -> throw(badarg)
    end;

parse_date(Str) when is_list(Str) -> parse_date(iolist_to_binary(Str)).

%%--------------------------------------------------------------------

%% @doc
%% Parse time of day. E.g., `{@module}:parse_time(<<"09:38:14">>)'
%% will result in `{9, 38, 14}' term. Function throws atom `badarg'
%% for syntactically incorrect input and throws `badtime' when invalid
%% time value parsed.
%%
%% Leap seconds are allowed along with any valid hour and minute and
%% will be mapped to second 59. E.g., term `{9, 38, 59}' will be
%% result of `{@module}:parse_time(<<"09:38:60">>)'.
%% @end
%% @throws badarg | badtime
-spec parse_time(iodata()) -> time().

parse_time(Str) when is_binary(Str) ->
    case parse_time_part(Str) of
        {Time, <<>>} -> Time;
        {_Time, _StrLeft} -> throw(badarg)
    end;

parse_time(Str) when is_list(Str) -> parse_time(iolist_to_binary(Str)).

%%%===================================================================
%%% API
%%%===================================================================

-ifndef(pre18).

%% @equiv format_system_time(SysTime, _Unit = native)
-spec format_system_time(non_neg_integer()) -> iodata().

format_system_time(SysTime) -> format_system_time(SysTime, native).

%%--------------------------------------------------------------------

%% @doc
%% Format Erlang system time.
%% @end
-spec format_system_time(non_neg_integer(), erlang:time_unit()) -> iodata().

format_system_time(SysTime, 1) ->
    format_datetime(system_seconds_to_datetime(SysTime));

format_system_time(SysTime, 1000) ->
    format_datetime(system_seconds_to_datetime(SysTime div 1000),
                    _Frac = {SysTime rem 1000, millisecond});

format_system_time(SysTime, 1000000) ->
    format_datetime(system_seconds_to_datetime(SysTime div 1000000),
                    _Frac = {SysTime rem 1000000, microsecond});

format_system_time(SysTime, 1000000000) ->
    format_datetime(system_seconds_to_datetime(SysTime div 1000000000),
                    _Frac = {SysTime rem 1000000000, nanosecond});

format_system_time(SysTime, native) ->
    format_system_time(SysTime, erlang:convert_time_unit(1, seconds, native));

format_system_time(SysTime, Unit)
  when Unit =:= second;
       Unit =:= seconds ->
    format_system_time(SysTime, 1);

format_system_time(SysTime, Unit)
  when Unit =:= millisecond;
       Unit =:= milli_seconds ->
    format_system_time(SysTime, 1000);

format_system_time(SysTime, Unit)
  when Unit =:= microsecond;
       Unit =:= micro_seconds ->
    format_system_time(SysTime, 1000000);

format_system_time(SysTime, Unit)
  when Unit =:= nanosecond;
       Unit =:= nano_seconds ->
    format_system_time(SysTime, 1000000000);

format_system_time(SysTime, PartsPerSecond) when PartsPerSecond < 1000 ->
    format_system_time(erlang:convert_time_unit(SysTime, PartsPerSecond, 1000), 1000);

format_system_time(SysTime, PartsPerSecond) when PartsPerSecond < 1000000 ->
    format_system_time(erlang:convert_time_unit(SysTime, PartsPerSecond, 1000000), 1000000);

format_system_time(SysTime, PartsPerSecond) when PartsPerSecond < 1000000000 ->
    format_system_time(erlang:convert_time_unit(SysTime, PartsPerSecond, 1000000000), 1000000000);

format_system_time(_SysTime, _Unit) -> error(badarg).

%%--------------------------------------------------------------------

%% @equiv parse_system_time(Str, _Unit = native)
-spec parse_system_time(iodata()) -> non_neg_integer().

parse_system_time(Str) -> parse_system_time(Str, native).

%%--------------------------------------------------------------------

%% @doc
%% Parse Erlang system time. For timestamps prior to
%% "1970-01-01T00:00:00Z" error atom `baddate' will be thrown, as
%% there's no way to represent such timestamp as POSIX time.
%% Conversion to requested time unit may result in accuracy
%% loss. E.g., timestamp has nanosecond fraction and conversion to
%% seconds was requested.
%% @end
%% @throws error()
-spec parse_system_time(iodata(), erlang:time_unit()) -> non_neg_integer().

parse_system_time(Str, Unit) ->
    {SysTime, FromUnit} =
        case parse_datetime(Str) of
            {{Date, Time}, Frac} when Date >= {1970, 1, 1} ->
                Seconds = datetime_to_system_seconds({Date, Time}),
                case Frac of
                    undefined -> {Seconds, seconds};
                    {N, millisecond} -> {Seconds * 1000 + N, milli_seconds};
                    {N, microsecond} -> {Seconds * 1000000 + N, micro_seconds};
                    {N, nanosecond} -> {Seconds * 1000000000 + N, nano_seconds}
                end;
            _PreEpoch -> throw(baddate)
        end,
    erlang:convert_time_unit(SysTime, FromUnit, Unit).

-endif. % pre18

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec format2(0..99) -> iodata().

format2(N) ->
    <<(N div 10 + $0),
      (N rem 10 + $0)>>.

%%--------------------------------------------------------------------

-spec format3(0..999) -> iodata().

format3(N) ->
    <<(N div 100 + $0),
      (N rem 100 div 10 + $0),
      (N rem 10 + $0)>>.

%%--------------------------------------------------------------------

-spec format4(0..9999) -> iodata().

format4(N) ->
    <<(N div 1000 + $0),
      (N rem 1000 div 100 + $0),
      (N rem 100 div 10 + $0),
      (N rem 10 + $0)>>.

%%--------------------------------------------------------------------

-spec format6(0..999999) -> iodata().

format6(N) ->
    [format4(N div 100),
     format2(N rem 100)].

%%--------------------------------------------------------------------

-spec format9(0..999999999) -> iodata().

format9(N) ->
    [format6(N div 1000),
     format3(N rem 1000)].

%%--------------------------------------------------------------------

-spec format_fraction(fraction()) -> iodata().

format_fraction({N, millisecond}) when N < 1000 -> format3(N);
format_fraction({N, microsecond}) when N < 1000000 -> format6(N);
format_fraction({N, nanosecond}) when N < 1000000000 -> format9(N);
format_fraction(N) when N >= 0, N < 1000000 -> format6(N);
format_fraction(_Frac) -> error(badarg).

%%--------------------------------------------------------------------

-spec format_offset(offset() | undefined) -> iodata().

format_offset(undefined) -> <<"-00:00">>;

format_offset({0, 0}) -> <<"Z">>;

format_offset({Hours, Minutes}) ->
    [case Hours < 0 of
         true -> [$-, format2(-Hours)];
         false -> [$+, format2(Hours)]
     end, $:, format2(Minutes)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec digits_to_integer(48..57) -> 0..9.

digits_to_integer(A) -> A - $0.

%%--------------------------------------------------------------------

-spec digits_to_integer(48..57, 48..57) -> 0..99.

digits_to_integer(A, B) ->
    digits_to_integer(A) * 10 + digits_to_integer(B).

%%--------------------------------------------------------------------

-spec digits_to_integer(48..57, 48..57, 48..57, 48..57) -> 0..9999.

digits_to_integer(A, B, C, D) ->
    digits_to_integer(A) * 1000 + digits_to_integer(B) * 100 +
        digits_to_integer(C) * 10 + digits_to_integer(D).

%%--------------------------------------------------------------------

-spec parse_date_part(binary()) -> {date(), binary()}.

parse_date_part(<<Y3, Y2, Y1, Y0, $-,
                 M1, M0, $-,
                 D1, D0,
                 Str/bytes>>)
  when ?IS_DIGITS(Y3, Y2, Y1, Y0),
       ?IS_DIGITS(M1, M0),
       ?IS_DIGITS(D1, D0) ->
    Date = {digits_to_integer(Y3, Y2, Y1, Y0),
            digits_to_integer(M1, M0),
            digits_to_integer(D1, D0)},
    case calendar:valid_date(Date) of
        true -> {Date, Str};
        false -> throw(baddate)
    end;

parse_date_part(_BadStr) -> throw(badarg).

%%--------------------------------------------------------------------

-spec parse_time_part(binary()) -> {time(), binary()}.

parse_time_part(<<H1, H0, $:,
                 M1, M0, $:,
                 S1, S0,
                 Str/bytes>>)
  when ?IS_DIGITS(H1, H0),
       ?IS_DIGITS(M1, M0),
       ?IS_DIGITS(S1, S0) ->
    case {digits_to_integer(H1, H0),
          digits_to_integer(M1, M0),
          digits_to_integer(S1, S0)} of
        {Hour, Minute, 60}
          when Hour =< 23, Minute =< 59 -> {{Hour, Minute, 59}, Str};
        Time = {Hour, Minute, Second}
          when Hour =< 23, Minute =< 59, Second =< 59 -> {Time, Str};
        _BadTime -> throw(badtime)
    end;

parse_time_part(_BadStr) -> throw(badarg).

%%--------------------------------------------------------------------

-spec parse_frac_part(binary()) -> {fraction(), binary()}.

parse_frac_part(Str) -> parse_frac_part(Str, _Frac = 0, _FracLen = 0).

%%--------------------------------------------------------------------

-spec parse_frac_part(binary(), non_neg_integer(), non_neg_integer()) ->
                             {fraction(), binary()}.

parse_frac_part(<<D, Str/bytes>>, Frac, FracLen) when ?IS_DIGITS(D) ->
    parse_frac_part(Str, 10 * Frac + (D - $0), FracLen + 1);

parse_frac_part(Str, Frac, FracLen) when FracLen > 0 ->
    {case FracLen of
         1 -> {Frac * 100, millisecond};
         2 -> {Frac * 10, millisecond};
         3 -> {Frac, millisecond};
         4 -> {Frac * 100, microsecond};
         5 -> {Frac * 10, microsecond};
         6 -> {Frac, microsecond};
         7 -> {Frac * 100, nanosecond};
         8 -> {Frac * 10, nanosecond};
         9 -> {Frac, nanosecond};
         _ -> throw(badfrac)
     end, Str};

parse_frac_part(_Str, _Frac, _FracLen) -> throw(badarg).

%%--------------------------------------------------------------------

-spec parse_offset_part(binary()) -> {offset() | undefined, binary()}.

parse_offset_part(<<Z, Str/bytes>>) when Z =:= $Z; Z =:= $z -> {{0, 0}, Str};

parse_offset_part(<<"-00:00", Str/bytes>>) -> {undefined, Str};

parse_offset_part(<<Sign, H1, H0, $:, M1, M0, Str/bytes>>)
  when ?IS_DIGITS(H1, H0),
       ?IS_DIGITS(M1, M0) ->
    case {digits_to_integer(H1, H0),
          digits_to_integer(M1, M0)} of
        {Hour, Minute}
          when Hour =< 23, Minute =< 59 ->
            case Sign of
                $- -> {{-Hour, Minute}, Str};
                $+ -> {{Hour, Minute}, Str};
                _ -> throw(badarg)
            end;
        _BadOffset -> throw(badoffset)
    end;

parse_offset_part(_BadStr) -> throw(badarg).

%%--------------------------------------------------------------------

-spec remove_offset(datetime(), offset()) -> datetime().

remove_offset(DateTime, {0, 0}) -> DateTime;

remove_offset(DateTime, {Hours, Minutes}) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DateTime) -
          (Hours * 3600 + Minutes * 60)
     ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifndef(pre18).

-spec datetime_to_system_seconds(calendar:datetime1970()) -> non_neg_integer().

datetime_to_system_seconds(DateTime) ->
    Epoch = {{1970, 1, 1}, {0, 0, 0}},
    calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds(Epoch).

%%--------------------------------------------------------------------

-spec system_seconds_to_datetime(non_neg_integer()) -> calendar:datetime1970().

system_seconds_to_datetime(Seconds) ->
    Epoch = {{1970, 1, 1}, {0, 0, 0}},
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(Epoch) + Seconds
     ).

-endif. % pre18

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

format_datetime_1_test_() ->
    [ ?_assertEqual(<<"1995-11-13T13:27:08Z">>,
                    iolist_to_binary(format_datetime({{1995, 11, 13}, {13, 27, 8}})))
    ].

format_datetime_2_test_() ->
    [ ?_assertEqual(<<"1970-01-01T00:00:00.000001Z">>,
                    iolist_to_binary(format_datetime({{1970, 1, 1}, {0, 0, 0}}, 1))),
      ?_assertError(badarg, format_datetime({{1970, 1, 1}, {0, 0, 0}}, 1000000))
    ].

format_datetime_3_test_() ->
    [ ?_assertEqual(<<"1970-01-01T00:00:00.001Z">>,
                    iolist_to_binary(format_datetime({{1970, 1, 1}, {0, 0, 0}}, {1, millisecond}))),
      ?_assertEqual(<<"1970-01-01T00:00:00.000001Z">>,
                    iolist_to_binary(format_datetime({{1970, 1, 1}, {0, 0, 0}}, {1, microsecond}))),
      ?_assertEqual(<<"1970-01-01T00:00:00.000000001Z">>,
                    iolist_to_binary(format_datetime({{1970, 1, 1}, {0, 0, 0}}, {1, nanosecond})))
    ].

format_local_datetime_2_test_() ->
    [ ?_assertEqual(<<"2016-06-20T09:38:15+03:00">>,
                    iolist_to_binary(format_local_datetime(
                                       {{2016, 06, 20}, {09, 38, 15}}, {3, 0}))),
      ?_assertEqual(<<"2016-02-12T11:02:09-00:00">>,
                    iolist_to_binary(format_local_datetime(
                                       {{2016, 02, 12}, {11, 02, 09}}, undefined))),
      ?_assertEqual(<<"2016-02-12T01:02:03Z">>,
                    iolist_to_binary(format_local_datetime(
                                       {{2016, 02, 12}, {01, 02, 03}}, {0, 0}))),
      ?_assertEqual(<<"9999-12-31T23:59:59+23:59">>,
                    iolist_to_binary(format_local_datetime(
                                       {{9999, 12, 31}, {23, 59, 59}}, {23, 59})))
    ].

format_local_datetime_3_test_() ->
    [ ?_assertEqual(<<"1999-09-15T16:47:01.004567-11:30">>,
                    iolist_to_binary(format_local_datetime({{1999, 09, 15}, {16, 47, 1}},
                                                           {-11, 30}, 4567)))
    ].

format_date_1_test_() ->
    [ ?_assertEqual(<<"0000-01-01">>, iolist_to_binary(format_date({0, 1, 1}))),
      ?_assertEqual(<<"9999-12-31">>, iolist_to_binary(format_date({9999, 12, 31}))),
      ?_assertEqual(<<"2017-02-29">>, iolist_to_binary(format_date({2017, 02, 29}))),
      ?_assertEqual(<<"2016-06-20">>, iolist_to_binary(format_date({2016, 06, 20}))) ].

format_time_1_test_() ->
    [ ?_assertEqual(<<"00:00:00">>, iolist_to_binary(format_time({0, 0, 0}))),
      ?_assertEqual(<<"23:59:59">>, iolist_to_binary(format_time({23, 59, 59}))),
      ?_assertEqual(<<"09:38:15">>, iolist_to_binary(format_time({09, 38, 15}))) ].

parse_date_1_test_() ->
    [ ?_assertThrow(badarg, parse_date(<<>>)),
      ?_assertThrow(badarg, parse_date(<<"1970-01-XX">>)),
      ?_assertThrow(badarg, parse_date(<<"1970.01.01">>)),
      ?_assertThrow(badarg, parse_date(<<"+970-+1-+1">>)),
      ?_assertThrow(baddate, parse_date(<<"2017-02-29">>)),
      ?_assertEqual({1970, 1, 1}, parse_date(<<"1970-01-01">>)),
      ?_assertEqual({1970, 1, 1}, parse_date(["1970", $-, "01", $- | "01"]))
    ].

parse_time_1_test_() ->
    [ ?_assertThrow(badarg, parse_time(<<>>)),
      ?_assertThrow(badarg, parse_time(<<"22:13:??">>)),
      ?_assertThrow(badarg, parse_time(<<"22-13-57">>)),
      ?_assertThrow(badtime, parse_time(<<"22:13:75">>)),
      ?_assertThrow(badarg, parse_time(<<"+2:+3:+7">>)),
      ?_assertEqual({22, 13, 57}, parse_time(<<"22:13:57">>)),
      ?_assertEqual({22, 13, 57}, parse_time(["22", $:, <<"13:">> | "57"])),
      ?_assertEqual({23, 59, 59}, parse_time(<<"23:59:60">>))
    ].

parse_datetime_1_test_() ->
    DateTime = {{2017, 3, 3}, {13, 31, 37}},
    Ans = {DateTime, {523800, microsecond}},
    [ ?_assertMatch(Ans, parse_datetime(<<"2017-03-03T16:31:37.5238+03:00">>)),
      ?_assertMatch(Ans, parse_datetime(<<"2017-03-03t16:31:37.5238+03:00">>)),
      ?_assertMatch(Ans, parse_datetime(<<"2017-03-03 16:31:37.5238+03:00">>)),
      ?_assertThrow(badoffset, parse_datetime(<<"2017-03-03T16:31:37.5238-00:00">>)),
      ?_assertThrow(badarg, parse_datetime(<<"2017-03-03T16:31:37.5238?00:00">>)),
      ?_assertMatch(Ans, parse_datetime(<<"2017-03-03T13:31:37.5238Z">>)),
      ?_assertMatch(Ans, parse_datetime(<<"2017-03-03t13:31:37.5238z">>)),
      ?_assertMatch({DateTime, undefined},
                    parse_datetime(<<"2017-03-03T13:31:37Z">>)),
      ?_assertMatch({DateTime, {100, millisecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.1Z">>)),
      ?_assertMatch({DateTime, {120, millisecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.12Z">>)),
      ?_assertMatch({DateTime, {123, millisecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.123Z">>)),
      ?_assertMatch({DateTime, {123400, microsecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.1234Z">>)),
      ?_assertMatch({DateTime, {123450, microsecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.12345Z">>)),
      ?_assertMatch({DateTime, {123456, microsecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.123456Z">>)),
      ?_assertMatch({DateTime, {123456700, nanosecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.1234567Z">>)),
      ?_assertMatch({DateTime, {123456780, nanosecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.12345678Z">>)),
      ?_assertMatch({DateTime, {123456789, nanosecond}},
                    parse_datetime(<<"2017-03-03T13:31:37.123456789Z">>)),
      ?_assertThrow(badfrac, parse_datetime(<<"2017-03-03T13:31:37.1234567890Z">>)),
      ?_assertThrow(badarg, parse_datetime(<<"2017-03-03T13:31:37.Z">>)),
      ?_assertThrow(badarg, parse_datetime(<<"2017-03-03T13:31:37.+123Z">>)),
      ?_assertThrow(badarg, parse_datetime(<<"2017-03-03T13:31:37.-123Z">>)),
      ?_assertMatch({{{1996, 12, 20}, {0, 39, 57}}, undefined},
                    parse_datetime(<<"1996-12-19T16:39:57-08:00">>))
    ].

parse_local_datetime_1_test_() ->
    DateTime = {{2017, 3, 3}, {16, 31, 37}},
    Offset = {3, 0},
    Frac = {523800, microsecond},
    [ ?_assertMatch({DateTime, Offset, Frac},
                    parse_local_datetime(<<"2017-03-03T16:31:37.5238+03:00">>)),
      ?_assertMatch({DateTime, Offset, undefined},
                    parse_local_datetime(<<"2017-03-03T16:31:37+03:00">>)),
      ?_assertMatch({DateTime, undefined, Frac},
                    parse_local_datetime(<<"2017-03-03T16:31:37.5238-00:00">>)),
      ?_assertThrow(badarg, parse_local_datetime(<<"2017-03-03T16:31:37+03:+4">>)),
      ?_assertThrow(badarg, parse_local_datetime(<<"2017-03-03T16:31:37+03:XX">>)),
      ?_assertThrow(badarg, parse_local_datetime(<<"2017-03-03T16:31:37-+3:00">>)),
      ?_assertThrow(badarg, parse_local_datetime(<<"2017-03-03T16:31:37--3:00">>)),
      ?_assertThrow(badoffset, parse_local_datetime(<<"2017-03-03T16:31:37+42:00">>)),
      ?_assertMatch({{{1985, 4, 12}, {23, 20, 50}}, {0, 0}, {520, millisecond}},
                    parse_local_datetime(<<"1985-04-12T23:20:50.52Z">>))
    ].

-ifndef(pre18).

format_system_time_2_test_() ->
    [ ?_assertEqual(<<"1970-01-01T00:02:03Z">>,
                    iolist_to_binary(format_system_time(123, seconds))),
      ?_assertEqual(<<"1970-01-01T00:00:00.123Z">>,
                    iolist_to_binary(format_system_time(123, milli_seconds))),
      ?_assertEqual(<<"1970-01-01T00:00:00.000123Z">>,
                    iolist_to_binary(format_system_time(123, micro_seconds))),
      ?_assertEqual(<<"1970-01-01T00:00:00.000000123Z">>,
                    iolist_to_binary(format_system_time(123, nano_seconds)))
    ].

parse_system_time_1_test_() ->
    [ ?_assertEqual(123, parse_system_time(<<"1970-01-01T00:02:03Z">>, seconds)),
      ?_assertEqual(123, parse_system_time(<<"1970-01-01T00:00:00.123Z">>, milli_seconds)),
      ?_assertEqual(123, parse_system_time(<<"1970-01-01T00:00:00.000123Z">>, micro_seconds)),
      ?_assertEqual(123, parse_system_time(<<"1970-01-01T00:00:00.000000123Z">>, nano_seconds))
    ].

-endif. % pre18

-endif.
