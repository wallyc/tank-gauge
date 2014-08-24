-module(db). 
-author('wally.cash@gmail.com').
-compile(export_all).
-export([]).
-import(lists, [foreach/2]).
-include("gauge.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%----------------------------------------------------------------------
%% Mnesia/DB
%%----------------------------------------------------------------------

init() ->
	mnesia:stop(),
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(user,   [{disc_copies, [node()]}, {attributes, record_info(fields, user)}]),
	mnesia:create_table(tank,   [{disc_copies, [node()]}, {attributes, record_info(fields, tank)}]),
	mnesia:create_table(gauge,  [{disc_copies, [node()]}, {attributes, record_info(fields, gauge)}]),
	mnesia:create_table(product,	[{disc_copies, [node()]}, {attributes, record_info(fields, product)}]),
	load_data().

start() ->
    mnesia:start().

stop() ->
	mnesia:stop().

clear_tables() ->
    mnesia:clear_table(user),
	mnesia:clear_table(gauge),
	mnesia:clear_table(tank),
	mnesia:clear_table(product).

load_data() ->
    F = fun() ->
		foreach(fun mnesia:write/1, db_tables())
	end,
    mnesia:transaction(F).

db_tables() ->
    [
		{user, "Demo", "demo1234"},
		{product, 0, "none"},
		{product, 1, "PG 64-22"},
		{product, 2, "PG 70-22"},
		{product, 3, "PG 76-22"},
		{product, 4, "CRS-2L"},
		{product, 5, "#2 Fuel Oil"}
    ] ++ tank:init_metrics(). 

do(Q) ->
  	F = fun() -> qlc:e(Q) end,
  	{atomic, Val} = mnesia:transaction(F),
  	Val.

add_row(X) ->
  	F = fun() -> mnesia:write(X) end,
  	mnesia:transaction(F).

delete_row(T, X) ->
  	F = fun() -> mnesia:delete({T, X}) end,
  	mnesia:transaction(F).

reset(X) ->
  	mnesia:clear_table(X).

select_table(T) ->
  	do(qlc:q([X || X <- mnesia:table(T)])).

select_last_gauge(T) ->
	Q=do(qlc:q([X#gauge.id || X <- mnesia:table(gauge), X#gauge.tank =:= T])),
	case Q of 
		[] ->
			[[]];
		_ ->
			H = lists:last(lists:sort(Q)),
			do(qlc:q([X || X <- mnesia:table(gauge), X#gauge.id =:= H]))
	end.

select_tk_ids() ->
	do(qlc:q([X#tank.id || X <- mnesia:table(tank)])).

select_products() ->
  	do(qlc:q([X#product.name || X <- mnesia:table(product)])).

select_tank(T) ->
	do(qlc:q([X || X <- mnesia:table(tank), X#tank.id =:= T])).

update(Tab, Key, Value) ->
 	F = fun() ->
    	[P] = mnesia:wread({Tab, Key}),
    	mnesia:write(P#gauge{gauger=Value})
  	end,
  	mnesia:transaction(F).

update_record(T,G,U) ->
 	F = fun() ->
    	[P] = mnesia:wread({tank, T}),
    	mnesia:write(P#tank{grav = G, product = U})
  	end,  	
	case mnesia:transaction(F) of
		{atomic, ok} ->
			{"Success!", "Your changes have been saved."};
		_ ->
			{"Error!", "Your Changes have not been saved."}
	end.

