-module(eshetserv_tree_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    eshetsrv_tree:new().

example() ->
    #{a=>{leaf, leaf_a}, b=>#{c=>{leaf, leaf_c}, d=>{leaf, leaf_d}}}.

lookup_test() ->
    directory = eshetsrv_tree:lookup(example(), []),
    directory = eshetsrv_tree:lookup(example(), [b]),
    {leaf, leaf_a} = eshetsrv_tree:lookup(example(), [a]),
    {leaf, leaf_c} = eshetsrv_tree:lookup(example(), [b, c]),
    {leaf, leaf_d} = eshetsrv_tree:lookup(example(), [b, d]),
    nothing = eshetsrv_tree:lookup(example(), [c]),
    nothing = eshetsrv_tree:lookup(example(), [b, e]),
    {error, not_a_directory} = eshetsrv_tree:lookup(example(), [a, e]).

insert_basic_test() ->
    {ok, T} = eshetsrv_tree:insert(eshetsrv_tree:new(), [a], b),
    {leaf, b} = eshetsrv_tree:lookup(T, [a]).

insert_subdir_test() ->
    {ok, T} = eshetsrv_tree:insert(eshetsrv_tree:new(), [a, b], c),
    {leaf, c} = eshetsrv_tree:lookup(T, [a, b]).

insert_existing_subdir_test() ->
    {ok, T} = eshetsrv_tree:insert(example(), [b, e], f),
    {leaf, f} = eshetsrv_tree:lookup(T, [b, e]).

insert_errors_test() ->
    {error, path_is_directory} = eshetsrv_tree:insert(example(), [], x),
    {error, path_is_directory} = eshetsrv_tree:insert(example(), [b], x),
    {error, path_already_exists} = eshetsrv_tree:insert(example(), [a], x),
    {error, not_a_directory} = eshetsrv_tree:insert(example(), [a, e], x).

remove_test() ->
    ?assertEqual({ok, #{b=>#{c=>{leaf, leaf_c}, d=>{leaf, leaf_d}}}}, eshetsrv_tree:remove(example(), [a])),
    ?assertEqual({ok, #{a=>{leaf, leaf_a}, b=>#{d=>{leaf, leaf_d}}}}, eshetsrv_tree:remove(example(), [b, c])).

remove_empty_test() ->
    {ok, T} = eshetsrv_tree:remove(example(), [b, c]),
    {ok, T2} = eshetsrv_tree:remove(T, [b, d]),
    ?assertEqual(#{a=>{leaf, leaf_a}}, T2),

    {ok, T3} = eshetsrv_tree:remove(T2, [a]),
    ?assertEqual(#{}, T3).

remove_errors_test() ->
    {error, path_is_directory} = eshetsrv_tree:remove(example(), []),
    {error, path_is_directory} = eshetsrv_tree:remove(example(), [b]),
    {error, no_such_node} = eshetsrv_tree:remove(example(), [c]),
    {error, no_such_node} = eshetsrv_tree:remove(example(), [b, e]),
    {error, not_a_directory} = eshetsrv_tree:remove(example(), [a, e]),
    {error, no_such_node} = eshetsrv_tree:remove(example(), [c, e]).
