-module(tests). 
-author('wally.cash@gmail.com').
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

tests() ->	
	?assertEqual(13.5, gauge:to_decimal({1, 1, 0.5})),
	?assertEqual("1-1 1/2", gauge:to_pretty_gauge(13.5)),
	?assertEqual(true, gauge:is_valid_gauge(0, 0, 0)),
	?assertEqual(true, gauge:is_valid_gauge(0, 0, 0.5)),
	?assertEqual(true, gauge:is_valid_gauge(1, 0, 0.75)),
	?assertEqual(true, gauge:is_valid_gauge(1, 1, 0.5)),
	?assertEqual(false, gauge:is_valid_gauge(1, 13, 0.5)),
	?assertEqual(false, gauge:is_valid_gauge(1, 0, 1)),
	?assertEqual(false, gauge:is_valid_gauge(1, 0, 0.52)).


	


