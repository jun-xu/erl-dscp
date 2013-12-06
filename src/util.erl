%% Author: sunshine
%% Created: 2013-11-20
%% Description: TODO: Add description to hurr_util
-module(util).

%%
%% Include files
%%
-include("log.hrl").
-include("dscp.hrl").

%%
%% Exported Functions
%%
-export([tstamp/0,get_app_env/3,to_hex/1]).

%%
%% API Functions
%%

tstamp() ->
    {Mega, Sec, Micro} = now(),
    ((Mega * 1000000) + Sec)*1000 + Micro div 1000.


-spec get_app_env(Key::atom(),Opts::[tuple()],Default::any()) -> any().
get_app_env(Key, Opts, Default) ->
    case proplists:get_value(Key, Opts) of
        undefined ->
					case application:get_env(?APP_NAME,Key) of
                		{ok, Value} -> Value;
                		undefined ->  Default
            		end;
        Value ->
            Value
    end.

to_hex([]) -> [];
to_hex(Bin) when is_binary(Bin) ->
	to_hex(binary_to_list(Bin));
to_hex([H | T]) ->
	[to_digit(H div 16),to_digit(H rem 16) | to_hex(T)].


to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N -10.

%%
%% Local Functions
%%

