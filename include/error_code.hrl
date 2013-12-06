%% 

-define (NOERROR , 0).
-define (UNKOWN_ERROR , 500).
-define (NOFUNCTION , 501).
-define (EXIT_ERROR , 502).
-define (ENCODE_ERROR , 503).
-define (PARAMETER_ERROR , 505).
-define (NOT_FOUND,506).

-define(BAD_REQUEST,400).


-define(error_mapping,[
					   {exite,?EXIT_ERROR},
					   {not_found,?NOT_FOUND}
					  ]).
