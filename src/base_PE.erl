%% Author: sunshine
%% Created: 2013-11-21
%% Description: TODO: Add description to base_PE
-module(base_PE).

%%
%% Include files
%%
-include("log.hrl").
-include("dscp.hrl").
%%
%% Exported Functions
%%
-export([behaviour_info/1]).
-export([open/1,close/0,pause/0,start/0,claneup/0,ack/1,fail/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
    [{open,1},{close,0},{pause,0},{start,0},{claneup,0},{ack,1},{fail,1}
	];
behaviour_info(_) ->
    undefined.
%%
%% Local Functions
%%
open(_Arg) -> {error,not_inplement}.

start() -> {error,not_inplement}.
pause() -> {error,not_inplement}.
close() -> {error,not_inplement}.


claneup() -> {error,not_inplement}.
ack(_MsgId) -> {error,not_inplement}.
fail(_MsgId) -> {error,not_inplement}.
