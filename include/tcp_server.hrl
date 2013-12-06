
-define(LISTENQUEUE,1024).
-define(DEFAULT_PORT,60001).
-define(SOCKET_OPTS,[binary, {reuseaddr, true},
					 {keepalive, true}, {backlog, ?LISTENQUEUE}, {active, false}]).