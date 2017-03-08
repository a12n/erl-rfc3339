# Summary #

Erlang library to format and parse RFC 3339 dates and times.

It has functions to format and parse dates, time of day, timestamps
with UTC offset and millisecond, microseconds or nanosecond
fractions, Erlang system time. Leap seconds are handled.

Formatting doesn't use `io_lib:format` and thus is an order of
magnitude faster (in Erlang 18) than straightforward `io_lib:format`
formatting.

# Build and Install #

The library could be used from other applications using [rebar] or
[erlang.mk] as usual. Do yourself a favor and specify a particular tag
or commit identifier, do not track a branch. For `rebar` applications
add something like this to the `deps` in `rebar.config`:

```
{rfc3339, {hg, "https://bitbucket.org/a12n/erl-rfc3339", {ref, "63e8715"}}}
```

Build documentation with `make doc` command and view `doc/index.html`.

To try out library functions in Erlang shell `make shell` command may
be useful.

Use `make test` command to run the tests. It will build and run EUnit
and [PropEr] tests.

# Basic Usage #

The following example shows how formatting and parsing may look like
when used in Erlang shell:

```
1> DateTime = {{2016, 6, 20}, {9, 38, 14}},
1> UTCOffset = {3, 0},
1> Microseconds = 523400,
1> Str = iolist_to_binary(rfc3339:format_local_datetime(DateTime, UTCOffset, Microseconds)).
<<"2016-06-20T09:38:14.523400+03:00">>
```

```
2> {LocalDateTime, UTCOffset, Fraction} = rfc3339:parse_local_datetime(Str).
{{{2016,6,20},{9,38,14}},{3,0},{523400,microsecond}}
3> {UTCDateTime, Fraction} = rfc3339:parse_datetime(Str).
{{{2016,6,20},{6,38,14}},{523400,microsecond}}
```

For more information (millisecond and nanosecond fractions,
undefined UTC offset and leap seconds, parse error handling) see
documentation.

[rebar]: https://www.rebar3.org/docs/dependencies
[erlang.mk]: https://erlang.mk/guide/deps.html#_adding_dependencies_to_your_project
[PropEr]: https://github.com/manopapad/proper
