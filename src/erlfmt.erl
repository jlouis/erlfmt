-module(erlfmt).

%% API exports
-export([main/1, fmt/3]).

%%====================================================================
%% API functions
%%====================================================================
main([FileName]) ->
    case file:read_file(FileName) of
        {ok, Data} ->
            {ok, Translated} = translate(binary_to_list(Data)),
            {ok, Pretty} = fmt(Translated, 4, 80),
            io:format("~ts~n", [Pretty]),
            ok;
        {error, enoent} ->
            stderr("No such file"),
            halt(1)
    end;
main(_) ->
    usage(),
    halt(1).

usage() ->
    io:format("Usage: erlfmt <FILE>~n").

stderr(Str) ->
    io:format(standard_error, Str, []).

%% fmt/3 formats a string containing an erlang term in a pretty-printed way
fmt(String, Indent, MaxCol) ->
    try
        {ok, AbsTerm, _} = erl_scan:string(String ++ "."),
        {ok, Term} = erl_parse:parse_term(AbsTerm),
        RecF = fun(_A, _N) -> no end,
        PPStr =
            iolist_to_binary(
              io_lib_pretty:print(Term, Indent, MaxCol, -1, -1, RecF)),
        {ok, PPStr}
    catch
        _:Error ->
            {error, Error}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% translate/1 makes the input string parsable
%%
%% Erlang output is not isomorphic in the sense you cannot parse it
%% again. One of the many reasons are that Ports, Pids and References
%% cannot be read into a VM again in this textual format. Hence we fix
%% that by wrapping such things in single quotes, making them into
%% atoms for the parser to chew on.
%%
%% The truly isomorphic thing are term_to_binary/binary_to_term, but
%% they are not human-readable and rarely in log lines.
translate(Data) ->
    Res1 = re:replace(Data, "\\.\\.\\.", "'&'", [{return, list},
                                                 global]),
    Res2 = re:replace(Res1, "<\\d+\\.\\d+\\.\\d+>", "'&'", [{return, list},
                                                            global]),
    Res3 = re:replace(Res2, "#Port<\\d+\\.\\d+>", "'&'", [{return, list},
                                                          global]),
    Res4 = re:replace(Res3, "#Ref<(\\d+\\.)+\\d+>", "'&'", [{return, list},
                                                            global]),
    {ok, Res4}.

