%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%		Also see : http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles.
%%% Created : 2011-10-25
%%% -------------------------------------------------------------------
-module(tcp_client_handler).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,set_socket/2,close_socket/1,send/2,stop/2,async_send/2]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
                socket,    % client socket
                addr,       % client address
				buff = undefined
               }).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_socket(Pid,Socket) when is_pid(Pid), is_port(Socket)->
    gen_server:cast(Pid,{set_socket,Socket}).

close_socket(Pid) ->
	gen_server:call(Pid,close_socket).

send(Pid,Bin) ->
	gen_server:call(Pid,{send,Bin},infinity).

async_send(Pid,Bin) ->
	gen_server:cast(Pid,{send,Bin}).

stop(Pid,Reason) ->
	gen_server:call(Pid,{stop,Reason}).

%% ====================================================================
%% Server functions
%% ====================================================================

%%----------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc
%%      Initializes the server
%% @end
%%----------------------------------------------------------------------
init([]) ->
	process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @doc
%% 		Handling call messages
%% @end
%%--------------------------------------------------------------------

handle_call({stop,Reason}, _From, #state{socket=Socket} = State) ->
    ok = gen_tcp:close(Socket),
	{stop, Reason, ok, State};
  
handle_call({send,Bin}, _From, #state{socket=Socket} = State) ->
    Replay = gen_tcp:send(Socket, Bin),
	{reply, Replay, State};

handle_call(close_socket, _From, #state{socket=Socket} = State) ->
   	gen_tcp:close(Socket),
    {stop, normal, ok ,State};
handle_call(_, _, State) ->
	{reply, ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc
%% 		Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({set_socket,Socket}, State) ->
	inet:setopts(Socket, [{active, once}, binary]),
	{ok, {Ip, _Port}} = inet:peername(Socket),
    {noreply,State#state{socket=Socket,addr = Ip}};

handle_cast({send,Data},#state{socket=Socket} = State) ->
	case gen_tcp:send(Socket, Data) of
		ok ->
			{noreply, State};
		{error,E} ->
			?DEBUG("~p -- send data to client error by reason:~p,stop.~n",[?MODULE,E]),
			{stop,normal,State}
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc
%% 		Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{socket=Socket} = State) ->
	%% dispatcher request and wait response.
	case request_dispatcher:dispatcher(self(),Bin) of
		{reply,Data} ->
			case gen_tcp:send(Socket, Data) of
				ok ->
					% Flow control: enable forwarding of next TCP message
					inet:setopts(Socket, [{active, once}]),
					{noreply,State};
				{error,E} ->
					?ERROR("~p -- send data fail by reason:~p ~n",[?MODULE,E]),
					{stop,E,State}
			end;
		noreply ->
			% Flow control: enable forwarding of next TCP message
			inet:setopts(Socket, [{active, once}]),
			{noreply,State};
		{error,Error} ->
			?ERROR("~p cmd dispatcher fails by reason:~p~n",[?MODULE,Error]),
			{stop,Error,State}
	end;

handle_info({tcp_closed, Socket}, #state{socket=Socket} = StateData) ->
    {stop, normal, StateData};

handle_info({send_and_close,Data}, #state{socket=Socket} = State) ->
	?ERROR("~p cmd dispatcher fails by reason:~p~n",[?MODULE,send_and_close]),
	case gen_tcp:send(Socket, Data) of
		ok ->
			{stop,normal,State};
		{error,E} ->
			{stop,E,State}
	end;

handle_info(_Info, State) ->
	{noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{socket=Socket}) ->
    ?DEBUG("~p -- tcp client handler:~p terminate by reason:~p~n",[?MODULE,self(),Reason]),
	(catch gen_tcp:close(Socket)),
    ok.
%%-------------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

dispatcher_cmd(Socket,Bin) ->
	%% dispatcher request and wait response.
	case request_dispatcher:dispatcher(self(),Bin) of
		{reply,Data} ->
			case gen_tcp:send(Socket, Data) of
				ok ->
					% Flow control: enable forwarding of next TCP message
					inet:setopts(Socket, [{active, once}]),
					ok;
%% 					{noreply,State};
				{error,E} ->
					?ERROR("~p -- send data fail by reason:~p ~n",[?MODULE,E]),
					{error,E}
			end;
		noreply ->
			% Flow control: enable forwarding of next TCP message
			inet:setopts(Socket, [{active, once}]),
			ok;
%% 			{noreply,State};
		{error,Error} ->
			?ERROR("~p cmd dispatcher fails by reason:~p~n",[?MODULE,Error]),
			{stop,Error}
	end.