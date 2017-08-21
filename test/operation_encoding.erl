-module (operation_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pair_ops_test() ->
    Ops = [{fst, increment}, {snd, {fst, increment}},
           {snd, {add, 17}}, {snd, {fst, {add, 17}}},
           {snd, {apply, "key", {set, 1010, "value"}}}],
    mult_encode_decode(Ops,entry).

set_ops_test() ->
    Ops = [{add, a}, {add, "a"}],
    mult_encode_decode(Ops,entry).

map_ops_test() ->
    Ops = [{apply, "hey", 2}, {apply, "hi", "random_string"},
           {apply, "hello", can_it_handle_atoms},
           {apply, "top_level", {apply, "nested", 3}},
           {apply, "top_level", {apply, "nested1", "nested string"}},
           {apply, "top_level", {apply, "nested2", {add, 3}}}],
    mult_encode_decode(Ops,entry).

opget_test() ->
    Op = #opget{key = "my_lasp_kv_key", type = gcounter},
    encode_decode(Op, opget).

opupdate_test() ->
    Op = #opupdate{
            k=#opget{key = "my_lasp_kv_key", type = gcounter},
            e=increment,
            actor=node()
    },
    encode_decode(Op, opupdate).

req_opget_test() ->
    Op = #req{u = {get, #opget{key = "my_lasp_kv_key", type = gcounter}}},
    encode_decode(Op, req).

req_opupdate_inc_counter_test() ->
    Op = #req{u = {put,
                #opupdate{
                    k=#opget{key = "my_lasp_kv_key", type = gcounter},
                    e=increment,
                    actor=node()}}},
    encode_decode(Op, req).

mult_encode_decode([H|T], RecordType) ->
    true = encode_decode(H, RecordType),
    mult_encode_decode(T, RecordType);
mult_encode_decode([], _) ->
    true.

encode_decode(Op, RecordType) ->
    % io:format("original operation: ~p~n", [Op]),
    % io:format("internal encode(Op): ~p~n", [encode(Op)]),
    Encoded = lasp_pb:encode_msg(encode(Op)),
    % io:format("encoded form: ~p~n",[Encoded]),
    Decoded = decode(lasp_pb:decode_msg(Encoded, RecordType)),
    % io:format("decoded form: ~p~n",[Decoded]),
    true = Op =:= Decoded.

encode(#req{u = {put, OpUpdate}}) ->
    #req{u = {put, encode(OpUpdate)}};

encode(#req{u = {get, OpGet}}) ->
    #req{u = {get, encode(OpGet)}};

encode(#opupdate{k = GetOp, e = Entry, actor = Actor}) ->
    #opupdate{k = encode(GetOp),
              e = encode(Entry),
              actor = encode_atom(Actor)};

encode(#opget{key = K, type = T}) ->
    #opget{key = K,
           type = atom_to_list(T)};

encode({A, B}) ->
    #entry{u = {ii, #pair{a = encode(A),
                          b = encode(B)}}};
encode({A, B, C}) ->
    #entry{u = {iii, #triple{a = encode(A),
                            b = encode(B),
                            c = encode(C)}}};

encode(A) when is_integer(A) ->
    #entry{u = {int, A}};
encode(A) when is_atom(A) ->
    #entry{u = {atm, atom_to_list(A)}};
% else assume it's string
encode(A) ->
    #entry{u = {str, A}}.

encode_atom(A) when is_atom(A) ->
    atom_to_list(A).

%% Decode operations.

decode(#req{u = {get, OpGet}}) ->
    #req{u = {get, decode(OpGet)}};

decode(#req{u = {put, OpUpdate}}) ->
    #req{u = {put, decode(OpUpdate)}};

decode(#opget{key = K, type = T}) ->
    #opget{key = binary_to_list(K),
           type = decode_atom(T)};

decode(#opupdate{k = GetOp, e = Entry, actor = Actor}) ->
    #opupdate{k = decode(GetOp),
              e = decode(Entry),
              actor = decode_atom(Actor)};

decode(#entry{u = {int, A}}) ->
    A;
decode(#entry{u = {atm, A}}) ->
    binary_to_atom(A, utf8);
decode(#entry{u = {str, A}}) ->
    binary_to_list(A);
decode(#entry{u = {ii, #pair{a = A,
                             b = B}}}) ->
    {decode(A), decode(B)};
decode(#entry{u = {iii, #triple{a = A,
                               b = B,
                               c = C}}}) ->
    {decode(A), decode(B), decode(C)}.

decode_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8).
-endif.
