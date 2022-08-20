-module(eshetsrv_tree).
-export([new/0]).
-export([lookup/2]).
-export([list_dir/2]).
-export([insert/3]).
-export([remove/2]).
-export([update/3]).
-export([map/2]).

-type tree() :: map().
-type path() :: [any()].

new() -> #{}.

-spec lookup(tree(), [any()]) ->
    {leaf, any()} | directory | nothing | {error, not_a_directory}.
lookup(Tree, Path) ->
    case Path of
        [] ->
            directory;
        [Leaf] ->
            case Tree of
                #{Leaf := {leaf, Value}} ->
                    {leaf, Value};
                #{Leaf := _} ->
                    directory;
                #{} ->
                    nothing
            end;
        [Head | Tail] ->
            case Tree of
                #{Head := {leaf, _Value}} ->
                    {error, not_a_directory};
                #{Head := Dir} ->
                    lookup(Dir, Tail);
                #{} ->
                    nothing
            end
    end.

-spec list_dir(tree(), [any()]) ->
    {ok, [{any(), dir | {leaf, any()}}]} | {error, not_a_directory}.
list_dir(Tree, []) ->
    {ok, [
        case Entry of
            #{} -> {Name, dir};
            {leaf, Leaf} -> {Name, {leaf, Leaf}}
        end
     || {Name, Entry} <- maps:to_list(Tree)
    ]};
list_dir(Tree, [Head | Tail]) ->
    case Tree of
        #{Head := {leaf, _Value}} ->
            {error, not_a_directory};
        #{Head := Dir} ->
            list_dir(Dir, Tail);
        #{} ->
            {error, not_a_directory}
    end.

-spec insert(tree(), path(), any()) ->
    {ok, tree()}
    | {error, path_already_exists}
    | {error, path_is_directory}
    | {error, not_a_directory}.
insert(Tree, Path, Value) ->
    case Path of
        [] ->
            {error, path_is_directory};
        [Leaf] ->
            case Tree of
                #{Leaf := {leaf, _Value}} ->
                    {error, path_already_exists};
                #{Leaf := _} ->
                    {error, path_is_directory};
                #{} ->
                    {ok, Tree#{Leaf => {leaf, Value}}}
            end;
        [Head | Tail] ->
            case Tree of
                #{Head := {leaf, _Value}} ->
                    {error, not_a_directory};
                #{Head := Dir} ->
                    case insert(Dir, Tail, Value) of
                        {ok, NewDir} -> {ok, Tree#{Head => NewDir}};
                        Err -> Err
                    end;
                #{} ->
                    case insert(#{}, Tail, Value) of
                        {ok, NewDir} -> {ok, Tree#{Head => NewDir}};
                        Err -> Err
                    end
            end
    end.

-spec put(tree(), path(), any()) ->
    {ok, tree()}
    | {error, path_is_directory}
    | {error, not_a_directory}.
put(Tree, Path, Value) ->
    case Path of
        [] ->
            {error, path_is_directory};
        [Leaf] ->
            case Tree of
                #{Leaf := {leaf, _Value}} ->
                    {ok, Tree#{Leaf => {leaf, Value}}};
                #{Leaf := _} ->
                    {error, path_is_directory};
                #{} ->
                    {ok, Tree#{Leaf => {leaf, Value}}}
            end;
        [Head | Tail] ->
            case Tree of
                #{Head := {leaf, _Value}} ->
                    {error, not_a_directory};
                #{Head := Dir} ->
                    case put(Dir, Tail, Value) of
                        {ok, NewDir} -> {ok, Tree#{Head => NewDir}};
                        Err -> Err
                    end;
                #{} ->
                    case put(#{}, Tail, Value) of
                        {ok, NewDir} -> {ok, Tree#{Head => NewDir}};
                        Err -> Err
                    end
            end
    end.

-spec remove(tree(), path()) ->
    {ok, tree()}
    | {error, path_is_directory}
    | {error, not_a_directory}
    | {error, no_such_node}.
remove(Tree, Path) ->
    case Path of
        [] ->
            {error, path_is_directory};
        [Leaf] ->
            case Tree of
                #{Leaf := {leaf, _Value}} ->
                    {ok, maps:remove(Leaf, Tree)};
                #{Leaf := _} ->
                    {error, path_is_directory};
                #{} ->
                    {error, no_such_node}
            end;
        [Head | Tail] ->
            case Tree of
                #{Head := {leaf, _Value}} ->
                    {error, not_a_directory};
                #{Head := Dir} ->
                    case remove(Dir, Tail) of
                        {ok, NewDir} when NewDir =:= #{} -> {ok, maps:remove(Head, Tree)};
                        {ok, NewDir} -> {ok, Tree#{Head => NewDir}};
                        Err -> Err
                    end;
                #{} ->
                    {error, no_such_node}
            end
    end.

-type update_fun() :: fun((nothing | {leaf, any()}) -> {leaf, any()} | nothing).
-type update_fun_ret() :: fun((nothing | {leaf, any()}) -> {leaf, any(), any()} | {nothing, any()}).
-spec update(tree(), path(), update_fun() | update_fun_ret()) ->
    {ok, tree()}
    | {ok, tree(), any()}
    | {error, path_is_directory}
    | {error, not_a_directory}.
update(Tree, Path, Fun) ->
    Action =
        case lookup(Tree, Path) of
            nothing -> {call, nothing};
            {leaf, Leaf} -> {call, {leaf, Leaf}};
            directory -> {error, path_is_directory};
            {error, not_a_directory} -> {error, not_a_directory}
        end,

    case Action of
        {call, Arg} ->
            case Fun(Arg) of
                {leaf, NewLeaf} ->
                    put(Tree, Path, NewLeaf);
                nothing ->
                    remove(Tree, Path);
                {leaf, NewLeaf, Ret} ->
                    {Type, Value} = put(Tree, Path, NewLeaf),
                    {Type, Value, Ret};
                {nothing, Ret} ->
                    {Type, Value} = remove(Tree, Path),
                    {Type, Value, Ret};
                {error, E} ->
                    {error, E}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec map(tree(), fun((path(), any()) -> {leaf, any()} | nothing)) -> tree().
map(Tree, Fun) ->
    case map(Tree, Fun, []) of
        nothing -> new();
        NewTree -> NewTree
    end.

map(Tree, Fun, Path) when is_map(Tree) ->
    Entries = [
        {Key, map(Value, Fun, Path ++ [Key])}
     || {Key, Value} <- maps:to_list(Tree)
    ],
    EntriesNotNothing = [
        {Key, Value}
     || {Key, Value} <- Entries,
        Value =/= nothing
    ],
    case EntriesNotNothing of
        [] -> nothing;
        _ -> maps:from_list(EntriesNotNothing)
    end;
map({leaf, L}, Fun, Path) ->
    Fun(Path, L).
