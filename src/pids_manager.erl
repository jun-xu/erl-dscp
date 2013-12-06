%%% -------------------------------------------------------------------
%%% Author  : sunshine
%%% Description :
%%%
%%% Created : 2012-8-9
%%% -------------------------------------------------------------------
-module(pids_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("log.hrl").

-define(PIDS_MAPPING_TAB,pids_mapping).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,stop/0,register_pid/2,get_pid_by_name/1,unregister_pid/2,get_all_pids/0]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pids}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_pid_by_name(Name) ->
	gen_server:call(?MODULE, {get_pid_by_name,Name}).

register_pid(Name,Pid) ->
	gen_server:call(?MODULE, {register_pid,Name,Pid}).

unregister_pid(Name,Pid) ->
	gen_server:call(?MODULE, {unregister_pid,Name,Pid}).

get_all_pids() ->
	gen_server:call(?MODULE, get_all_pids).

stop() ->
	gen_server:call(?MODULE, stop).
	

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
	?INFO("~p -- start...~n",[?MODULE]),
	Ets = ets:new(?PIDS_MAPPING_TAB, [set,private]),
    {ok, #state{pids=Ets}}.

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

handle_call({get_pid_by_name,Name}, _From, #state{pids=Ets} = State) ->
	case ets:lookup(Ets, Name) of
		[] ->
			{reply, undefined, State};
		[{Name,_Ref,Pid}] ->
			{reply, Pid, State}
	end;
	

handle_call({unregister_pid,Name,Pid}, _From, #state{pids=Ets} = State) ->
	case ets:lookup(Ets, Name) of
		[] ->
			{reply, ok, State};
		[{Name,Ref,Pid}] ->
			erlang:demonitor(Ref,[flush]),
			ets:delete(Ets,Name),
			{reply, ok, State};
		_ ->
			{reply, {error,"pid_error"}, State}
	end;	

handle_call({register_pid,Name,Pid}, _From, #state{pids=Ets} = State) when is_pid(Pid)->
	Ref = erlang:monitor(process, Pid),
	case ets:insert_new(Ets, {Name,Ref,Pid}) of
		true ->
			{reply, ok, State};
		false ->
			{reply, {error,"already_start"}, State}
	end;	

handle_call({register_pid,_Name,_Pid}, _From, State) ->
	{reply, {error,bad_pid}, State};

handle_call(get_all_pids, _From, #state{pids=Ets} = State) ->
	Pids = ets:tab2list(Ets),
    {reply, {ok,Pids}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
	
handle_call(Request, _From, State) ->
	?INFO("~p -- not implement request:~p",[?MODULE,Request]),
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
handle_info({'DOWN',Ref,process,_Pid,_Info},#state{pids=Ets} = State) ->
	true = ets:match_delete(Ets,{'_',Ref,'_'}),
    {noreply, State};

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
terminate(Reason, _State) ->
	?INFO("~p -- terminate by reason:~p",[?MODULE,Reason]),
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

