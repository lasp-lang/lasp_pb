-module(lasp_pb_codec).

-include ("lasp_pb.hrl").

%% API
-export([ encode/1, decode/1]).

%% Testing API
-export ([ encode_decode/2, mult_encode_decode/2 ]).

%% Defines for state encoding
-define (CRDT_COUNTERS, [bcounter, gcounter, pncounter, lexcounter]).
-define (CRDT_FLAGS, [ewflag, dwflag]).
-define (CRDT_SETS, [gset, awset, rwset, twopset, orset, gset]).
-define (CRDT_REGISTERS, [mvregister, lwwregister]).
-define (CRDT_MAPS, [mvmap, gmap, awmap]).

%% Encoding operations
encode({req, {ReqType, Req}}) -> #req{u = {ReqType, encode({ReqType, Req})}};
encode({get, {Key, KeyType}}) -> #opget{key = Key, type = atom_to_list(KeyType)};
encode({put, {Kid, Entry, Actor}}) -> #opupdate{k = encode({get, Kid}),
                                                e = encode(Entry),
                                                actor = atom_to_list(Actor)};

encode({response, Arg, Response}) ->
    case get_crdt_type(Arg) of
        undefined -> #reqresp{v = {Arg, encode_status(Arg, Response)}};
        Type -> #reqresp{v = {Type, encode_state(Type, Response)}}
    end;

encode({A, B, C, D}) -> #entry{u = {iv, #quad{a = encode(A), b = encode(B), c = encode(C), d = encode(D)}}};
encode({A, B, C}) -> #entry{u = {iii, #triple{a = encode(A), b = encode(B), c = encode(C)}}};
encode({A, B}) -> #entry{u = {ii, #pair{a = encode(A), b = encode(B)}}};



encode(A) when is_atom(A) -> #entry{u = {atm, atom_to_list(A)}};
encode(A) when is_integer(A) -> #entry{u = {int, A}};
encode(A) when is_list(A) ->
    case io_lib:printable_unicode_list(A) of
        true -> #entry{u = {str, A}};
        false -> #entry{u = {list, #list{elems = [encode(E) || E <- A]}}}
    end.

encode_status(success, true) -> 1;
encode_status(success, false) -> 0;

encode_status(error, Reason) when is_atom(Reason) -> atom_to_list(Reason);
encode_status(error, Reason) when is_list(Reason) -> Reason.

encode_state(ctr, Val) when is_integer(Val) -> #valcounter{val = Val};

encode_state(reg, Val) when is_atom(Val) -> #valreg{val = atom_to_list(Val)};
encode_state(reg, Val) when is_list(Val) -> #valreg{val = Val};

encode_state(flag, _) -> error(not_implemented);

encode_state(set, []) ->
    #valset{elems = []};
encode_state(set, Elems) when is_list(Elems), is_atom(hd(Elems)) ->
    #valset{elems = lists:map(fun(E) -> atom_to_list(E) end, Elems)};
encode_state(set, Elems) when is_list(Elems), is_list(hd(Elems)) ->
    #valset{elems = Elems};

encode_state(map, []) ->
    #valmap{entries = []};
encode_state(map, Entries) when is_list(Entries), is_tuple(hd(Entries)), tuple_size(hd(Entries)) =:= 3 ->
    #valmap{entries = lists:map(fun(Entry) ->
        {Key, Type, Val} = Entry,
        EntryType = get_crdt_type(Type),
        EncEntryVal = encode_state(EntryType, Val),
        #mapentry{key = Key, ktype = atom_to_list(Type), val = #mapfield{v = {EntryType, EncEntryVal}}}
    end, Entries)}.

get_crdt_type(Type) ->
    Types = [
        {ctr, ?CRDT_COUNTERS},
        {reg, ?CRDT_REGISTERS},
        {flag, ?CRDT_FLAGS},
        {set, ?CRDT_SETS},
        {map, ?CRDT_MAPS}
    ],
    search_type(Type,Types).

search_type(_Type, []) ->
    undefined;
search_type(Type, [H|T]) ->
    {ReturnType, List} = H,
    case lists:member(Type, List) of
        true -> ReturnType;
        false -> search_type(Type, T)
    end.

decode(#reqresp{v = {error, Reason}}) ->      {response, error, binary_to_list(Reason)};
decode(#reqresp{v = {success, Success}}) ->   {response, success, Success};
decode(#reqresp{v = {Type, Record}}) ->      {response, decode(Record)};

decode(#valreg{val = Value}) when is_atom(Value) ->     Value;
decode(#valreg{val = Value}) when is_binary(Value) ->   binary_to_list(Value);

decode(#valcounter{val = Value}) -> Value;

decode(#valset{elems = Elems}) -> decode_string_list(Elems);

decode(#valmap{entries = Entries}) -> lists:map(fun decode/1, Entries);

decode(#mapentry{key = K, ktype = KT, val = V}) -> {binary_to_list(K), binary_to_atom(KT, utf8), decode(V)};

decode(#mapfield{v = {_, Value}}) -> decode(Value);

decode(#req{u = {_, Record}}) -> {req, decode(Record)};

decode(#opget{key = K, type = T}) -> {get, {binary_to_list(K), decode_atom(T)}};

decode(#opupdate{k = #opget{key = K, type = T}, e = Entry, actor = Actor}) ->
    {put, { {binary_to_list(K), binary_to_atom(T, utf8)},
            decode(Entry),
            decode_atom(Actor)}};

decode(#entry{u = {int, Val}}) -> Val;
decode(#entry{u = {atm, BinAtom}}) -> binary_to_atom(BinAtom, utf8);
decode(#entry{u = {str, BinStr}}) -> binary_to_list(BinStr);
decode(#entry{u = {list, #list{elems = Elems}}}) -> lists:map(fun decode/1, Elems);

decode(#entry{u = {ii, #pair{a = A, b = B}}}) -> {decode(A), decode(B)};
decode(#entry{u = {iii, #triple{a = A, b = B, c = C}}}) -> {decode(A), decode(B), decode(C)};
decode(#entry{u = {iv, #quad{a = A, b = B, c = C, d = D}}}) -> {decode(A), decode(B), decode(C), decode(D)};

decode(true) -> true;
decode(false) -> false.

decode_atom(Bin) when is_binary(Bin) ->
    binary_to_atom(Bin, utf8).

decode_string_list(List) ->
    decode_string_list(List, []).

decode_string_list([], Res) ->
    lists:reverse(Res);
decode_string_list([H|T], Res) ->
    H1 = binary_to_list(H),
    decode_string_list(T, [H1|Res]).

%% -------------------------------------------------------------------
%% Testing/utility functions
%% -------------------------------------------------------------------

encode_decode(Op, RecordType) ->
    % io:format("original operation: ~p~n", [Op]),
    io:format("internal encode(Op): ~p~n", [encode(Op)]),
    Encoded = lasp_pb:encode_msg(encode(Op)),
    % io:format("encoded form: ~p~n",[Encoded]),
    Decoded = decode(lasp_pb:decode_msg(Encoded, RecordType)),
    io:format("decoded form: ~p~n",[Decoded]),
    Decoded.

mult_encode_decode(Ops, RecordType) -> mult_encode_decode(Ops, RecordType, []).

mult_encode_decode([], _, Result) -> lists:reverse(Result);

mult_encode_decode([H|T], RecordType, Result) ->
    mult_encode_decode(T, RecordType, [encode_decode(H, RecordType) | Result]).
