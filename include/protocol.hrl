

-record(protocol,{cmd,
				  ack_cmd,
				  encode = undefined,
				  decode = undefined,
				  handler = undefined
				 }).

%% -define(PREVIEW_CMD,16#00018001).
%% -define(ACK_CMD(CMD),16#80000000+CMD).

-define(request_route_map,[
%% 	 {?PREVIEW_CMD,#protocol{cmd=?PREVIEW_CMD,ack_cmd=?ACK_CMD(?PREVIEW_CMD),
%% 							 decode = undefined,
%% 							 encode = undefined,
%% 							 handler = {smts_dvr_manager,preview}
%% 						}
%% 	 }
	]).





