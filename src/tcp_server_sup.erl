%%% -------------------------------------------------------------------
%%% Author  : xujun
%%% Description :
%%%		Also see : http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles
%%% Created : 2011-10-25
%%% -------------------------------------------------------------------
-module(tcp_server_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("tcp_server.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_client/0,start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================
%% A startup function for spawning new client connection handling connect.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(tcp_client_sup, []).

start_link() ->
    ListenPort = util:get_app_env(listen_port,[],?DEFAULT_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, tcp_client_handler]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_listener_server,                          
                  {tcp_listener_server,start_link,[Port,Module]}, 
                  permanent,                               
                  2000,                                  
                  worker,                             
                  [tcp_listener_server]                         
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module]]},
                  permanent,                              
                  infinity,                               
                  supervisor,                              
                  []                                   
              }
            ]
        }
    };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                             
                  {Module,start_link,[]},                 
                  temporary,                            
                  2000,                                 
                  worker,                               
                  []                                   
              }
            ]
        }
    }.

%% ====================================================================
%% Internal functions
%% ====================================================================

