
-define(APP_NAME,dscp).

%% state 
-define(JOB_STATE_INIT,init).
-define(JOB_STATE_START,start).
-define(JOB_STATE_PAUSE,pause).
-define(JOB_STATE_STOP,stop).

%% report for every spouts and bolt.
-record(report,{min = 0,
				max = 0,
				avg = 0}).

%% topology
%% -record(topology,{}).
