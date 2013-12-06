%% Author: sunshine
%% Created: 2011-10-25
%% Description: TODO: Add description to request_dispatcher
-module(request_dispatcher).

%%
%% Include files
%%
-include("protocol.hrl").
-include("error_code.hrl").
-include("log.hrl").

%%
%% Exported Functions
%%
-export ([dispatcher/2,handle_protocol/3]).

%%
%% API Functions
%%

dispatcher(Pid,Bin) ->
	<<Cmd:32,B/binary>> = Bin,
	case lists:keysearch(Cmd, 1, ?request_route_map) of
		{value,{Cmd,Protocol}} ->
			handle_protocol(Pid,Protocol,B);
		false ->
			?DEBUG("~p -- can not dispatcher of cmd:~p~n",[?MODULE,Cmd]),
			response_error(Cmd + 16#80000000,?NOFUNCTION,atom_to_binary('not_implemented',utf8))
	end.

%%
%% Local Functions
%%

handle_protocol(SocketPid,#protocol{cmd=Cmd,decode=DecodeFun,handler=HandlerFun,encode=EncodeFun,ack_cmd=ACKCMD},B) ->
	Msg = decode_protocol(DecodeFun,B),
	Reply = handle_msg(SocketPid,HandlerFun,Msg),
	case ACKCMD of
		undefined ->
			handle_reply(Cmd + 16#80000000,EncodeFun,Reply);
		_ ->
			handle_reply(ACKCMD,EncodeFun,Reply)
	end.

decode_protocol(undefined,B) ->
	B;
decode_protocol({M,F},B) ->
	M:F(B).

handle_msg(_,undefined,_) ->
	{error,?NOFUNCTION,atom_to_binary('not_implemented',utf8)};
handle_msg(SocketPid,{M1,F1},Msg) ->
	case catch(M1:F1(SocketPid,Msg)) of
		{'EXIT', Reason} -> 
			?ERROR("~p -- handle msg fail by reason:~p~n",[?MODULE,Reason]),
			{error,exite};
		Reply -> 
%% 			?DEBUG("~p -- handler replay:~p~n",[?MODULE,Reply]),
			Reply
	end.

handle_reply(_,_,noreply) ->
	noreply;
handle_reply(ACKCMD,_,{error,Reason}) ->
	case proplists:get_value(Reason, ?error_mapping) of
		undefined -> response_error(ACKCMD,?UNKOWN_ERROR);
		ErrorCode -> response_error(ACKCMD,ErrorCode)
	end;

handle_reply(ACKCMD,undefined,{error,Code,ReplyData}) ->
	response_error(ACKCMD,Code,ReplyData);
		
handle_reply(ACKCMD,_,ok) ->
	{reply,<<ACKCMD:32,?NOERROR :32>>};
handle_reply(ACKCMD,undefined,{ok,ReplyData}) ->
	{reply,[<<ACKCMD:32,?NOERROR:32>>,ReplyData]};
handle_reply(ACKCMD,EncodeFun,{ok,ReplyData}) ->
%% 	?INFO_F("encode data : ReplyData=~p~n",[ReplyData]),
	case encode_reply(EncodeFun,ReplyData) of
		{error,_Code,_Reason} ->
			response_error(ACKCMD,?ENCODE_ERROR);
		B ->
			{reply,[<<ACKCMD:32,?NOERROR:32>>,B]}
	end.

encode_reply({M,F},Data) ->
	case catch(M:F(Data)) of
		{'EXIT', Reason} -> 
			?INFO("encode data error : {M:~p,F:~p,A:~p},~p~n",[M,F,Data,Reason]),
			{error,?ENCODE_ERROR,encode_error};
		B ->
			B
	end.

response_error(ACKCMD,ErrorCode,ReplyData) ->
%% 	B = error_response_pb:encode_error_response(#error_response{errorcode=ErrorCode,reason=Reason}),
	{reply,[<<ACKCMD:32,ErrorCode:32>>,ReplyData]}.

response_error(ACKCMD,ErrorCode) ->
	{reply,<<ACKCMD:32,ErrorCode:32>>}.

