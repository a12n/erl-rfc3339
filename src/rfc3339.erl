-module(rfc3339).

%% Types
-export_type([year/0, month/0, day/0, date/0,
              hour/0, minute/0, second/0, time/0,
              fraction/0, fraction_unit/0,
              datetime/0, offset/0,
              error/0]).

%% API
-export([format_datetime/1, format_datetime/2, format_datetime/3,
         format_local_datetime/2, format_local_datetime/3, format_local_datetime/4,
         format_date/1,
         format_time/1, format_time/2, format_time/3]).

%% API
-export([parse_datetime/1,
         parse_local_datetime/1,
         parse_date/1,
         parse_time/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type year() :: calendar:year().
-type month() :: calendar:month().
-type day() :: calendar:day().

-type hour() :: calendar:hour().
-type minute() :: calendar:minute().
-type second() :: 0..60.                        % Allows leap second

-type fraction() :: non_neg_integer().
-type fraction_unit() :: millisecond | microsecond | nanosecond.

-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.
-type datetime() :: {date(), time()}.

-type offset() :: {-23..23, minute()}.

-type error() :: badstr | baddate | badtime | badfrac | badoffset.

%%%===================================================================
%%% API
%%%===================================================================

-spec format_datetime(datetime()) -> iodata().

format_datetime(DateTime) ->
    format_local_datetime(DateTime, _Offset = {0, 0}).

%%--------------------------------------------------------------------

-spec format_datetime(datetime(), fraction()) -> iodata().

format_datetime(DateTime, Frac) ->
    format_datetime(DateTime, Frac, _Unit = microsecond).

%%--------------------------------------------------------------------

-spec format_datetime(datetime(), fraction(), fraction_unit()) -> iodata().

format_datetime(DateTime, Frac, Unit) ->
    format_local_datetime(DateTime, _Offset = {0, 0}, Frac, Unit).

%%--------------------------------------------------------------------

-spec format_local_datetime(datetime(), offset() | undefined) -> iodata().

format_local_datetime({Date, Time}, Offset) ->
    [format_date(Date), $T, format_time(Time), format_offset(Offset)].

%%--------------------------------------------------------------------

-spec format_local_datetime(datetime(), offset() | undefined, fraction()) -> iodata().

format_local_datetime(DateTime, Offset, Frac) ->
    format_local_datetime(DateTime, Offset, Frac, _Unit = microsecond).

%%--------------------------------------------------------------------

-spec format_local_datetime(datetime(), offset() | undefined,
                            fraction(), fraction_unit()) -> iodata().

format_local_datetime({Date, Time}, Offset, Frac, Unit) ->
    [format_date(Date), $T, format_time(Time, Frac, Unit), format_offset(Offset)].

%%--------------------------------------------------------------------

-spec format_date(date()) -> iodata().

format_date({Year, Month, Day}) ->
    [format4(Year), $-, format2(Month), $-, format2(Day)].

%%--------------------------------------------------------------------

-spec format_time(time()) -> iodata().

format_time({Hour, Minute, Second}) ->
    [format2(Hour), $:, format2(Minute), $:, format2(Second)].

%%--------------------------------------------------------------------

-spec format_time(time(), fraction()) -> iodata().

format_time(Time, Frac) ->
    format_time(Time, Frac, _Unit = microsecond).

%%--------------------------------------------------------------------

-spec format_time(time(), fraction(), fraction_unit()) -> iodata().

format_time(Time, Frac, Unit) ->
    [format_time(Time), $., format_fraction(Frac, Unit)].

%%%===================================================================
%%% API
%%%===================================================================

-spec parse_datetime(iodata()) -> {datetime(), fraction(), fraction_unit()}.

parse_datetime(Str) ->
    case parse_local_datetime(Str) of
        {_Local, _Offset = undefined, _Frac, _Unit} -> throw(badoffset);
        {Local, Offset, Frac, Unit} -> {remove_offset(Local, Offset), Frac, Unit}
    end.

%%--------------------------------------------------------------------

-spec parse_local_datetime(iodata()) ->
                                  {datetime(), offset() | undefined,
                                   fraction(), fraction_unit()}.

parse_local_datetime(Str) when is_binary(Str) ->
    parse_date(
      Str,
      fun(<<Sep, TimeStr/bytes>>, Date)
            when Sep =:= $T; Sep =:= $t; Sep =:= $\s ->
              parse_time(
                TimeStr,
                fun(<<$., FracStr/bytes>>, Time) ->
                        parse_frac(
                          FracStr,
                          fun(OffsetStr, {FracLen, Frac}) ->
                                  Unit = case FracLen of
                                             3 -> millisecond;
                                             6 -> microsecond;
                                             9 -> nanosecond;
                                             _ -> throw(badfrac)
                                         end,
                                  parse_offset(
                                    OffsetStr,
                                    fun(_EmptyStr = <<>>, Offset) -> {{Date, Time}, Offset, Frac, Unit};
                                       (_StrLeft, _Offset) -> throw(badstr)
                                    end
                                   )
                          end
                         );
                   (OffsetStr, Time) ->
                        Frac = 0,
                        Unit = microsecond,
                        parse_offset(
                          OffsetStr,
                          fun(_EmptyStr = <<>>, Offset) -> {{Date, Time}, Offset, Frac, Unit};
                             (_StrLeft, _Offset) -> throw(badstr)
                          end
                         )
                end
               );
         (_BadStr, _Date) -> throw(badstr)
      end
     );

parse_local_datetime(Str) when is_list(Str) ->
    parse_local_datetime(iolist_to_binary(Str)).

%%--------------------------------------------------------------------

-spec parse_date(iodata()) -> date().

parse_date(Str) when is_binary(Str) ->
    parse_date(
      Str,
      fun(_EmptyStr = <<>>, Date) -> Date;
         (_StrLeft, _Date) -> throw(badstr)
      end
     );

parse_date(Str) when is_list(Str) -> parse_date(iolist_to_binary(Str)).

%%--------------------------------------------------------------------

-spec parse_time(iodata()) -> time().

parse_time(Str) when is_binary(Str) ->
    parse_time(
      Str,
      fun(_EmptyStr = <<>>, Time) -> Time;
         (_StrLeft, _Time) -> throw(badstr)
      end
     );

parse_time(Str) when is_list(Str) -> parse_time(iolist_to_binary(Str)).

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

-spec format_fraction(fraction(), fraction_unit()) -> iodata().

format_fraction(N, millisecond) -> format3(N rem 1000);
format_fraction(N, microsecond) -> format6(N rem 1000000);
format_fraction(N, nanosecond) -> format9(N rem 1000000000).

%%--------------------------------------------------------------------

-spec format_offset(offset() | undefined) -> iodata().

format_offset(undefined) -> <<"-00:00">>;

format_offset({0, 0}) -> <<"Z">>;

format_offset({Hours, Minutes}) ->
    [case Hours < 0 of
         true -> [$-, format2(-Hours)];
         false -> [$+, format2(Hours)]
     end, $:, format2(Minutes)].

%%--------------------------------------------------------------------

-spec parse_date(binary(), fun()) -> no_return().

parse_date(<<YearStr:4/bytes, $-,
             MonthStr:2/bytes, $-,
             DayStr:2/bytes,
             Str/bytes>>,
           Cont) ->
    Date = {binary_to_integer(YearStr),
            binary_to_integer(MonthStr),
            binary_to_integer(DayStr)},
    case calendar:valid_date(Date) of
        true -> Cont(Str, Date);
        false -> throw(baddate)
    end;

parse_date(_BadStr, _Cont) -> throw(badstr).

%%--------------------------------------------------------------------

-spec parse_time(binary(), fun()) -> no_return().

parse_time(<<HourStr:2/bytes, $:,
             MinuteStr:2/bytes, $:,
             SecondStr:2/bytes,
             Str/bytes>>,
           Cont) ->
    case {binary_to_integer(HourStr),
          binary_to_integer(MinuteStr),
          binary_to_integer(SecondStr)} of
        Time = {Hour, Minute, Second}
          when Hour >= 0, Hour =< 23,
               Minute >= 0, Minute =< 59,
               Second >= 0, Second =< 60 -> Cont(Str, Time);
        _BadTime -> throw(badtime)
    end;

parse_time(_BadStr, _Cont) -> throw(badstr).

%%--------------------------------------------------------------------

-spec parse_frac(binary(), fun()) -> no_return().

parse_frac(Str, Cont) -> parse_frac(Str, _Ans = {0, 0}, Cont).

%%--------------------------------------------------------------------

-spec parse_frac(binary(), term(), fun()) -> no_return().

parse_frac(<<D, Str/bytes>>, {FracLen, Frac}, Cont) when D >= $0, D =< $9 ->
    parse_frac(Str, {FracLen + 1, 10 * Frac + (D - $0)}, Cont);

parse_frac(Str, Ans = {FracLen, _Frac}, Cont) when FracLen > 0 -> Cont(Str, Ans);

parse_frac(_Str, _Ans, _Cont) -> throw(badstr).

%%--------------------------------------------------------------------

-spec parse_offset(binary(), fun()) -> no_return().

parse_offset(<<Z, Str/bytes>>, Cont) when Z =:= $Z; Z =:= $z -> Cont(Str, {0, 0});

parse_offset(<<"-00:00", Str/bytes>>, Cont) -> Cont(Str, undefined);

parse_offset(<<Sign, HourStr:2/bytes, $:, MinuteStr:2/bytes, Str/bytes>>, Cont) ->
    %% TODO: handle error:badarg from binary_to_integer/1
    Hour = binary_to_integer(HourStr),
    Minute = binary_to_integer(MinuteStr),
    case Sign of
        $- -> Cont(Str, {-Hour, Minute});
        $+ -> Cont(Str, {Hour, Minute})
    end;

parse_offset(_BadStr, _Cont) -> throw(badstr).

%%--------------------------------------------------------------------

-spec remove_offset(datetime(), offset()) -> datetime().

remove_offset(DateTime, {Hours, Minutes}) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(DateTime) -
          (Hours * 3600 + Minutes * 60)
     ).

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
                    iolist_to_binary(format_datetime({{1970, 1, 1}, {0, 0, 0}}, 1)))
    ].

format_local_datetime_2_test_() ->
    [ ?_assertEqual(<<"2016-06-20T09:38:15+03:00">>,
                    iolist_to_binary(format_local_datetime({{2016, 06, 20}, {09, 38, 15}}, {3, 0}))),
      ?_assertEqual(<<"2016-02-12T11:02:09-00:00">>,
                    iolist_to_binary(format_local_datetime({{2016, 02, 12}, {11, 02, 09}}, undefined))),
      ?_assertEqual(<<"2016-02-12T01:02:03Z">>,
                    iolist_to_binary(format_local_datetime({{2016, 02, 12}, {01, 02, 03}}, {0, 0}))),
      ?_assertEqual(<<"9999-12-31T23:59:60+23:59">>,
                    iolist_to_binary(format_local_datetime({{9999, 12, 31}, {23, 59, 60}}, {23, 59})))
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
      ?_assertEqual(<<"23:59:60">>, iolist_to_binary(format_time({23, 59, 60}))),
      ?_assertEqual(<<"09:38:15">>, iolist_to_binary(format_time({09, 38, 15}))) ].

format_time_2_test_() ->
    [ ?_assertEqual(<<"00:00:00.000123">>, iolist_to_binary(format_time({00, 00, 00}, 123))),
      ?_assertEqual(<<"23:59:59.999999">>, iolist_to_binary(format_time({23, 59, 59}, 999999))),
      ?_assertEqual(<<"23:59:59.000000">>, iolist_to_binary(format_time({23, 59, 59}, 0))) ].

-endif.
