-module(memhash_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, {no_auto_import, [get/1]}]).

-export([get/1,
         get_with_links/1,
         add/1,
         set/2,
         incr/1,
         decr/1]).

-opaque table_id() :: pos_integer().

-export_type([table_id/0]).

-spec seq_id() -> table_id().
seq_id() ->
    case erlang:get({seq, data}) of
        undefined ->
            erlang:put({seq, data}, 1),
            0;
        Num ->
            erlang:put({seq, data}, Num + 1),
            Num
    end.

-spec get(table_id()) -> term().
get(ID) ->
    {Value, _Links} = get_with_links(ID),
    Value.

-spec get_with_links(table_id()) -> {term(), pos_integer()}.
get_with_links(ID) ->
    erlang:get({data, ID}).

-spec add(Data :: term()) -> table_id().
add(Data) ->
    ID = seq_id(),
    erlang:put({data, ID}, {Data, 1}),
    ID.

-spec set(table_id(), Data :: term()) -> ok.
set(ID, Data) ->
    {_, Links} = get_with_links(ID),
    erlang:put({data, ID}, {Data, Links}),
    ok.

-spec incr(table_id()) -> ok.
incr(ID) ->
    {Value, Links} = erlang:get({data, ID}),
    erlang:put({data, ID}, {Value, Links + 1}),
    ok.

-spec decr(table_id()) -> ok.
decr(ID) ->
    {Value, Links} = erlang:get({data, ID}),
    if 
        Links =:= 1 -> erlang:erase({data, ID});
        true -> erlang:put({data, ID}, {Value, Links - 1})
    end,
    ok.
