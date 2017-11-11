-module(memhash).
-author('manuel@altenwald.com').

-export([new/0,
         get/2,
         rget/2,
         get_all/1,
         set_val/3,
         set_id/3,
         remove/2,
         destroy/1]).

-opaque memhash() :: reference().

-export_type([memhash/0]).

new() ->
    Ref = make_ref(),
    erlang:put({index, Ref}, []),
    Ref.

get(Ref, Key) ->
    Mem = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Mem) of
        false ->
            undefined;
        {value, Key, ID} when is_integer(ID) ->
            {value, ID, memhash_data:get(ID)}
    end.

rget(Ref, Key) ->
    case get(Ref, Key) of
        undefined ->
            undefined;
        {value, _ID, Result} when is_reference(Result) ->
            get_all(Result);
        {value, _ID, Result} ->
            Result
    end.

get_all(Ref) ->
    Indexes = erlang:get({index, Ref}),
    lists:map(fun({value, SubKey, ID}) ->
        case memhash_data:get(ID) of
            {value, _ID, Result} when is_reference(Result) ->
                {SubKey, get_all(Result)};
            {value, _ID, Result} ->
                {SubKey, Result}
        end
    end, Indexes).

set_val(Ref, Key, Value) ->
    Indexes = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Indexes) of
        false ->
            ID = memhash_data:add({value, Key, Value}),
            NewIndexes = [{value, Key, ID}|Indexes],
            erlang:put({index, Ref}, NewIndexes);
        {value, Key, ID} ->
            case memhash_data:get(ID) of
                {value, _ID, Result} when is_reference(Result) ->
                    destroy(Result);
                {value, _ID, _Result} ->
                    ok
            end,
            memhash_data:set(ID, {value, Key, Value})
    end,
    ok.

set_id(Ref, Key, ID) ->
    Indexes = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Indexes) of
        false ->
            memhash_data:incr(ID),
            erlang:put({index, Ref}, [{value, Key, ID}|Indexes]);
        {value, Key, ID2} ->
            memhash_data:decr(ID2),
            NewIndexes = [{value, Key, ID}|lists:keydelete(Key, 2, Indexes)],
            erlang:put({index, Ref}, NewIndexes)
    end,
    ok.

remove(Ref, Key) ->
    Indexes = erlang:get({index, Ref}),
    case lists:keyfind(Key, 2, Indexes) of
        false ->
            ok;
        {value, _Key, ID} ->
            memhash_data:decr(ID),
            erlang:put({index, Ref}, lists:keydelete(Key, 2, Indexes)),
            ok
    end.

%% TODO
destroy(_Ref) ->
    ok.
