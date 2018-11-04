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

