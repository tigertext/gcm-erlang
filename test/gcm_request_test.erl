-module(gcm_request_test).

-include_lib("eunit/include/eunit.hrl").

get_msg_type_ok_test() ->
    Msg1 = [{<<"type">>, <<"wakeUp">>}],
    ?assertEqual(<<"wakeUp">>, gcm_request:get_msg_type(Msg1)),
    Msg2 = [{<<"msg_type">>, <<"sleep">>}],
    ?assertEqual(<<"sleep">>, gcm_request:get_msg_type(Msg2)).

get_msg_type_undefined_test() ->
    Msg = [{<<"No_type">>, <<"wakeUp">>}],
    ?assertEqual(<<"undefined">>, gcm_request:get_msg_type(Msg)).

build_analytics_label_test() ->
    Msg = [{<<"type">>, <<"wakeUp">>}],
    MsgType = gcm_request:get_msg_type(Msg),
    ?assertEqual(
        [{<<"analytics_label">>, <<"gcm_erl_send%wakeUp">>}],
        gcm_request:build_analytics_label(MsgType, <<"gcm_erl_send">>)),
    Msg2 = [{<<"msg_type">>, <<"sleep">>}],
    MsgType2 = gcm_request:get_msg_type(Msg2),
    ?assertEqual(
        [{<<"analytics_label">>, <<"gcm_erl_send%sleep">>}],
        gcm_request:build_analytics_label(MsgType2, <<"gcm_erl_send">>)),
    Msg3 = [{<<"no_type">>, <<"sleep">>}],
    MsgType3 = gcm_request:get_msg_type(Msg3),
    ?assertEqual(
        [{<<"analytics_label">>, <<"gcm_erl_send%undefined">>}],
        gcm_request:build_analytics_label(MsgType3, <<"gcm_erl_send">>)).

