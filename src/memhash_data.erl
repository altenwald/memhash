-module(memhash_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, {no_auto_import, [get/1]}]).

-export([get/1,
         add/1,
         set/2,
         incr/1,
         decr/1]).

seq_id() ->
    case erlang:get({seq, data}) of
        undefined ->
            erlang:put({seq, data}, 1),
            0;
        Num ->
            erlang:put({seq, data}, Num + 1),
            Num
    end.

get(ID) ->
    {Value, _Links} = erlang:get({data, ID}),
    Value.

add(Data) ->
    ID = seq_id(),
    erlang:put({data, ID}, {Data, 1}),
    ID.

set(ID, Data) ->
    case erlang:get({data, ID}) of
        undefined -> erlang:put({data, ID}, {Data, 1});
        {_, Links} -> erlang:put({data, ID}, {Data, Links})
    end.

incr(ID) ->
    {Value, Links} = erlang:get({data, ID}),
    erlang:put({data, ID}, {Value, Links + 1}),
    ok.

decr(ID) ->
    {Value, Links} = erlang:get({data, ID}),
    if 
        Links =:= 1 -> erlang:erase({data, ID});
        true -> erlang:put({data, ID}, {Value, Links - 1})
    end,
    ok.
