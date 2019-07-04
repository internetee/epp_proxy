-module(epp_util).

-export([create_map/1, create_session_id/1,
         frame_length/1, frame_length_to_receive/1,
         frame_length_to_send/1, readable_ip/1, session_id/1]).

-define(OFFSET, 4).

%% Given a pid, return a sha512 hash of unique attributes.
-spec session_id(pid()) -> [char()].

session_id(Pid) ->
    UniqueMap = create_map(Pid),
    BinaryHash = create_session_id(UniqueMap),
    BinaryHash.

%% Give me a process id, I'll create a random map for you.
-spec create_map(pid()) -> #{string() => pid(),
                             string() => float(), string() => string()}.

create_map(Pid) when is_pid(Pid) ->
    Now = erlang:system_time(second),
    #{"pid" => Pid, "random" => rand:uniform(),
      "timestamp" => calendar:system_time_to_rfc3339(Now)}.

%% Given the special data structure, return back a binary hash to pass to the
%% application server.
-spec create_session_id(#{string() => pid(),
                          string() => float(),
                          string() => string()}) -> [char()].

create_session_id(#{"pid" := Pid, "random" := Random,
                    "timestamp" := Timestamp}) ->
    Map = #{"pid" => pid_to_list(Pid),
            "random" => float_to_list(Random),
            "timestamp" => Timestamp},
    ListOfTuples = maps:to_list(Map),
    ListOfLists = [[X, ",", Y] || {X, Y} <- ListOfTuples],
    NestedList = lists:join(",", ListOfLists),
    ListOfGlyphs = lists:flatten(NestedList),
    BinaryHash = crypto:hash(sha512, ListOfGlyphs),
    String = lists:flatten([integer_to_list(X, 16)
                            || <<X>> <= BinaryHash]),
    String.

frame_length_to_receive(Size) when Size >= 0 ->
    Size - (?OFFSET).

frame_length_to_send(Frame) ->
    Length = frame_length(Frame), Length + (?OFFSET).

frame_length(Frame) when is_binary(Frame) ->
    byte_size(Frame);
frame_length(Frame) when is_list(Frame) ->
    Bin = unicode:characters_to_binary(Frame),
    byte_size(Bin).

%% Pass a tuple of IP address, return a binary for sending over the wire.
-spec readable_ip({integer(), integer(), integer(),
                   integer()}) -> binary().

readable_ip({FirstOctet, SecondOctet, ThirdOctet,
             FourthOctet}) ->
    List = [integer_to_list(FirstOctet), ".",
            integer_to_list(SecondOctet), ".",
            integer_to_list(ThirdOctet), ".",
            integer_to_list(FourthOctet)],
    Binary = list_to_binary(List),
    Binary.
