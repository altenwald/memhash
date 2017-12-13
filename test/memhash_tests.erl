-module(memhash_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(undefined, memhash:get(M, <<"name">>)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    ?assertMatch({value, _Number, <<"Manuel">>}, memhash:get(M, <<"name">>)),
    ?assertMatch(ok, memhash:remove(M, <<"name">>)),
    ?assertEqual(undefined, memhash:get(M, <<"name">>)),
    ?assertEqual(ok, memhash:destroy(M)),
    ?assertEqual({error, enomemhash}, memhash:keys(M)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

remove_not_found_element_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertMatch(ok, memhash:remove(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

simple_with_refs_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    {value, ID, <<"Manuel">>} = memhash:get(M, <<"name">>),
    ?assertEqual(ok, memhash:set_id(M, <<"nombre">>, ID)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"name">>)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"nombre">>)),
    ok = memhash:remove(M, <<"name">>),
    ?assertMatch(undefined, memhash:get(M, <<"name">>)),
    ?assertMatch({value, ID, <<"Manuel">>}, memhash:get(M, <<"nombre">>)),
    ok = memhash:remove(M, <<"nombre">>),
    ?assertMatch(undefined, memhash:get(M, <<"name">>)),
    ?assertMatch(undefined, memhash:get(M, <<"nombre">>)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

overwriting_entries_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    {value, ID, <<"Manuel">>} = memhash:get(M, <<"name">>),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Angel">>)),
    ?assertMatch({value, ID, <<"Angel">>}, memhash:get(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

not_found_recursive_key_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(undefined, memhash:rget(M, <<"array">>)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

single_value_for_recursive_key_test() ->
    ok = memhash_data:start_link(),
    M = memhash:new(),
    ?assertEqual([], memhash:keys(M)),
    ?assertEqual(ok, memhash:set_val(M, <<"name">>, <<"Manuel">>)),
    ?assertEqual(<<"Manuel">>, memhash:rget(M, <<"name">>)),
    ok = memhash:destroy(M),
    ok = memhash_data:stop(),
    ok.

circular_error_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    {value, ID, _Array} = memhash:get(Base, <<"array">>),
    ?assertEqual(ok, memhash:set_id(Array, <<"loop">>, ID)),
    ?assertException(throw, {error, eloop}, memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok = memhash_data:stop(),
    ok.

circular_memhash_error_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Base)),
    ?assertException(throw, {error, eloop}, memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok = memhash_data:stop(),
    ok.

deep_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok = memhash_data:stop(),
    ok.

submemhash_cleaned_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ?assertEqual({error, enomemhash}, memhash:get(Array, 0)),
    ok = memhash_data:stop(),
    ok.

overwriting_reference_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    Array1 = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array1, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array1, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array1, 2, <<"hi again">>)),
    Array2 = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array2, 0, <<"hola">>)),
    ?assertEqual(ok, memhash:set_val(Array2, 1, <<"adios">>)),
    ?assertEqual(ok, memhash:set_val(Array2, 2, <<"hola de nuevo">>)),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array1)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array2)),
    ?assertEqual({error,enomemhash}, memhash:keys(Array1)),
    ?assertEqual([{0,<<"hola">>},{1,<<"adios">>},{2,<<"hola de nuevo">>}],
                 memhash:rget(Base, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ?assertEqual({error,enomemhash}, memhash:keys(Array2)),
    ok = memhash_data:stop(),
    ok.

id_test() ->
    ok = memhash_data:start_link(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    {value, ID, _Value} = memhash:get(Array, 0),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID)),
    ?assertEqual(ok, memhash:set_id(Array, 4, ID)),
    ?assertEqual(3, memhash_data:get_links(ID)),
    ?assertEqual(ok, memhash:remove(Array, 0)),
    ?assertEqual(2, memhash_data:get_links(ID)),
    ?assertEqual(ok, memhash:remove(Array, 4)),
    ?assertEqual(1, memhash_data:get_links(ID)),
    ?assertEqual([{1,<<"bye">>},{2,<<"hi again">>},{3,<<"hi">>}],
                 memhash:get_all(Array)),
    ?assertEqual(ok, memhash:destroy(Array)),
    ok = memhash_data:stop(),
    ok.

overwriting_id_test() ->
    ok = memhash_data:start_link(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Array, 0, <<"hi">>)),
    ?assertEqual(ok, memhash:set_val(Array, 1, <<"bye">>)),
    ?assertEqual(ok, memhash:set_val(Array, 2, <<"hi again">>)),
    {value, ID0, _Value0} = memhash:get(Array, 0),
    {value, ID1, _Value1} = memhash:get(Array, 1),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID0)),
    ?assertEqual(ok, memhash:set_id(Array, 3, ID1)),
    ?assertEqual([{0,<<"hi">>},{1,<<"bye">>},{2,<<"hi again">>},{3,<<"bye">>}],
                 memhash:get_all(Array)),
    ?assertEqual(1, memhash_data:get_links(ID0)),
    ?assertEqual(2, memhash_data:get_links(ID1)),
    ?assertEqual(ok, memhash:destroy(Array)),
    ok = memhash_data:stop(),
    ok.

doing_nasty_things_test() ->
    ok = memhash_data:start_link(),
    Base = memhash:new(),
    Array = memhash:new(),
    ?assertEqual(ok, memhash:set_val(Base, <<"array">>, Array)),
    ?assertEqual(ok, memhash:destroy(Array)),
    ?assertException(throw, {error, enomemhash}, memhash:rget(Base, <<"array">>)),
    ?assertException(throw, {error, enomemhash}, memhash:rget(Array, <<"array">>)),
    ?assertEqual(ok, memhash:destroy(Base)),
    ok = memhash_data:stop(),
    ok.
