


-define(ERROR(Msg),error_logger:error_msg(Msg)).
-define(ERROR(Format,Msg),error_logger:error_msg(Format,Msg)).
-define(INFO(Msg),error_logger:info_msg(Msg)).
-define(INFO(Format,Msg),error_logger:info_msg(Format,Msg)).

-define(DEBUG(Msg),error_logger:info_msg(Msg)).
-define(DEBUG(Format,Msg),error_logger:info_msg(Format,Msg)).
%% 
%% -define(INFO(Msg),ok).
%% -define(INFO_F(Format,Msg),ok).