-module (state_encoding).
-include("lasp_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

counter_val_test() ->
    Op = #valcounter{val=1234567890987654321},
    encode_decode(Op, valcounter).

set_val_test() ->
    Op = #valset{elems=["A", "bbb", "CCCcCCC"]},
    encode_decode(Op, valset).

reg_val_test() ->
    Op = #valreg{val = "Lasp rules"},
    encode_decode(Op, valreg).

map_val_test() ->
    Op =
        #valmap{
          entries = [
            #mapentry{key = "map_key",
                      ktype = gcounter,
                      val = #mapfield{
                        v = {c, #valcounter{val=1234567890987654321}}
                      }}
          ]},
    encode_decode(Op, valmap).

nested_map_val_test() ->
    Op = #valmap{
      entries = [
        #mapentry{key = "obj_key1", ktype = gcounter, val = #mapfield{
          v = {c, #valcounter{val = 1234567890987654321}}
        }},
        #mapentry{key = "obj_key2", ktype = orset, val = #mapfield{
          v = {s, #valset{elems = ["A", "B", "C"]}}
        }},
        #mapentry{key = "obj_key3", ktype = gmap, val = #mapfield{
          v = {m, #valmap{
                entries = [
                    #mapentry{key = "nested_obj_key1", ktype = gcounter,
                    val = #mapfield{v = {c, #valcounter{val = 123}}}},
                    #mapentry{key = "nested_obj_key2", ktype = orset,
                    val = #mapfield{v = {s, #valset{elems = ["ab", "cd"]}}}},
                    #mapentry{key = "nested_obj_key1", ktype = lwwregister,
                    val = #mapfield{v = {r, #valreg{val = "123"}}}}
                    ]}
                  }
        }}
      ]},
    encode_decode(Op, valmap).

%% Encoding
encode(#valmap{entries = ListEntries}) ->
    #valmap{entries = lists:map(fun encode/1,ListEntries)};

encode(#mapentry{key = K, ktype = KT, val = Val}) ->
    #mapentry{key = K, ktype = atom_to_list(KT), val = encode(Val)};

encode(#mapfield{v = {m, Map}}) ->
    #mapfield{v = {m, encode(Map)}};

%% Catch all clause
encode(A) ->
    A.

decode(#valmap{entries = ListEntries}) ->
    #valmap{entries = lists:map(fun decode/1,ListEntries)};

decode(#mapentry{key = K, ktype = KT, val = V}) ->
    #mapentry{key = binary_to_list(K),
              ktype = binary_to_atom(KT, utf8),
              val = decode(V)};

decode(#mapfield{v = {r, Reg}}) ->
    #mapfield{v = {r, decode(Reg)}};

decode(#mapfield{v = {m, Map}}) ->
    #mapfield{v = {m, decode(Map)}};

decode(#mapfield{v = {s, Set}}) ->
    #mapfield{v = {s, decode(Set)}};

decode(#mapfield{v = {c, Counter}}) ->
    #mapfield{v = {c, decode(Counter)}};

decode(#valreg{val=Val}) ->
    #valreg{val=binary_to_list(Val)};

decode(#valset{elems=ListElems}) ->
    #valset{elems=decode_string_list(ListElems)};

decode(#valcounter{val=Num}) when is_integer(Num) ->
    #valcounter{val = Num}.

decode_string_list(List) ->
    decode_string_list(List, []).

decode_string_list([], Res) ->
    lists:reverse(Res);
decode_string_list([H|T], Res) ->
    H1 = binary_to_list(H),
    decode_string_list(T, [H1|Res]).

encode_decode(Op, RecordType) ->
    io:format("original operation: ~p~n", [Op]),
    io:format("internal encode(Op): ~p~n", [encode(Op)]),
    Encoded = lasp_pb:encode_msg(encode(Op)),
    io:format("encoded form: ~p~n",[Encoded]),
    Decoded = decode(lasp_pb:decode_msg(Encoded, RecordType)),
    io:format("decoded form: ~p~n",[Decoded]),
    true = Op =:= Decoded.
-endif.
