-module(gauge). 
-author('wally.cash@gmail.com').
-compile(export_all).
-export([]).
-import(lists, [reverse/1,nth/2, foreach/2]).
-include("../../../../lib/yaws/include/yaws_api.hrl"). 
-include("gauge.hrl").
-include_lib("kernel/include/inet.hrl").

%%----------------------------------------------------------------------
%% Gauging/Measurement
%%----------------------------------------------------------------------

calc_gauge(A) ->
	G1 = calc_gauge2(A),
	case is_record(G1, gauge) of  
		true ->
			db:add_row(G1),
			gauge_result_body(G1);
		_ ->
			{_, E} = G1,
			alert("Error!", E)
	end.
 
calc_gauge2(G) ->
	[Tk] = tank:get_tank(G#gauge.tank),
	Cg = convert_gauge(Tk, G#gauge.gauge, G#gauge.type, G#gauge.hatch),
	case Cg of
		{ok, Cg2} ->
			Gr = tank:get_gallons(Cg2, tank:get_strap(G#gauge.tank), Tk#tank.critical, G#gauge.hatch),
			case Gr of
				{ok, Gr2} ->
					{Scf, Vcf} = case Tk#tank.insulated of 
						true ->
							factor:get_factors(G#gauge.grav, G#gauge.temp, G#gauge.table);
						_ ->
							factor:get_factors(G#gauge.grav, G#gauge.temp, G#gauge.ambient, G#gauge.table)
					end,
					{ok, A} = tank:get_gallons(Tk#tank.safe, tank:get_strap(G#gauge.tank), Tk#tank.critical, side),
					G#gauge{
						gauge_ht = tank:get_height(Tk, G#gauge.hatch),
						cgauge = Cg2,
						gross = Gr2,
						gross_x_scf = trunc(Scf * Gr2),
						net = trunc(Vcf * Scf * Gr2),
						scf = Scf,
						vcf = Vcf,
						safe_ht = Tk#tank.safe,
						product = Tk#tank.product,
						aspace = A - Gr2
					};
				_ ->
					Gr
			end;
		_ ->
			Cg
	end.

calc_stop(Arg) ->
	G = calc_gauge2(setup_args(Arg)),
	[Tk] = tank:get_tank(G#gauge.tank),
	Amt = to_integer(get_arg(Arg, "amount")) * ?BBL,
	G1 = parse_gauge(get_arg(Arg, "mobiscroll")),
	Act = list_to_atom(get_arg(Arg, "action")),
	calc_stop2(Arg, G, Tk, G1, Amt, Act).

calc_stop2(_Arg, _G, _Tk, _G1, Amt, _Act) when Amt =:= 0 ->
	alert("Error!", "Yeah, right! Enter a cargo value > 0...");

calc_stop2(_Arg, _G, _Tk, G1, _Amt, Act) when G1 =:= 0 , Act =:= stopout->
	alert("Error!", "You can't squeeze blood from a turnip. Tank is empty!");

calc_stop2(Arg, G, Tk, _G1, Amt, stopin) ->
	Grav1 = to_float(get_arg(Arg, "grav2")),
	Temp1 = to_float(get_arg(Arg, "temp2")),
	{Gross, Vcf} = net_to_gross(Amt, Temp1, Grav1, G#gauge.table),
	case Gross =< G#gauge.aspace of 
		true ->
			Temp2 = weighted_avg({G#gauge.net,G#gauge.temp},{G#gauge.net + Amt, Temp1}),
			Grav2 = weighted_avg({G#gauge.net,G#gauge.grav},{G#gauge.net + Amt, Grav1}),
			{Scf, Vcf2} = case Tk#tank.insulated of 
				true ->
					factor:get_factors(Grav2, Temp2, G#gauge.table);
				_ ->
					factor:get_factors(Grav2, Temp2, G#gauge.ambient, G#gauge.table)
			end,
			{Stop, V} = calc_stop3(G#gauge.gross + Gross, tank:get_strap(G#gauge.tank), Tk#tank.critical),
			Net1 = trunc((V * Scf * Vcf2) - G#gauge.net),
			sbody({G, Stop, Net1, Amt, Temp1, Grav1, Vcf, Temp2, Grav2});
		false ->
			alert("Error!", "Exceeds Safe Fill")
	end;

calc_stop2(_Arg, G, Tk, _G1, Amt, stopout) ->
	{Gross, _} = net_to_gross(Amt, G#gauge.temp, G#gauge.grav, G#gauge.table),
	case G#gauge.gross - Gross >= 0 of 
		true ->
			{Stop, V} = calc_stop3(G#gauge.gross - Gross, tank:get_strap(G#gauge.tank), Tk#tank.critical),
			Net1 = trunc(G#gauge.net-((V * G#gauge.scf) * G#gauge.vcf)),
			sbody({G, Stop, Net1, Amt});
		false ->
			alert("Error!", "Insufficient product")
	end.

calc_stop3(Target, S, C) ->
	{ok, V} = tank:get_gallons(0.0, S, C, side),
	calc_stop3(Target, S, Target - V, 0.0, C).

calc_stop3(_Target, _S, D, N, _C) when D =:= 0 -> 
	{N, 0};

calc_stop3(Target, S, D, N, C) when D > 0 -> 
	{ok, V} = tank:get_gallons(N + 1.0, S, C, side),
	calc_stop3(Target, S, Target - V, N + 1, C);

calc_stop3(_Target, S, D, N, C) when D < 0 -> 
	Stop = N + calc_fraction(D, S),
	{ok, V} = tank:get_gallons(Stop, S, C, side),
	{Stop, V}.


calc_fraction(X, _Y) when X == 0 -> 
	0;
calc_fraction(G, {X}) ->
	?SIXTEENTH * (trunc((G/X) / ?SIXTEENTH));
calc_fraction(G, {_X, _Y, [H|_]}) ->
	trunc(G/H) * ?SIXTEENTH.

gross_to_net(Gr, Temp, Grav, Table) ->
	Vcf = factor:get_vcf(Temp, Grav, Table),
	{trunc(Gr * Vcf), Vcf}.

net_to_gross(Net, Temp, Grav, Table) ->
	Vcf = factor:get_vcf(Temp, Grav, Table),
	{trunc(Net / Vcf), Vcf}.

convert_gauge(M, G, T, H) -> 
	Ht = tank:get_height(M, H),
  	case G =< Ht of
		true when G >= 0 ->
			case T of
				innage ->
					{ok, G};
				ullage ->
					{ok, Ht-G}
			end;
    	true when G < 0->
      		{error, "Gauge cannot be less than 0"} ;
		_ ->   
      		{error, "Gauge exceeds gauge height"}            
 	 end.

parse_gauge(G) ->
	case G of
		[] ->
			0;
		_ ->
			G1=string:tokens(G, " "),
			to_decimal({to_integer(nth(1,G1)), to_integer(nth(2,G1)), to_float(nth(3,G1))})
	end.

to_decimal({Ft, In, Fr}) ->
	Ft * 12 + In + Fr + 0.0.

to_pretty_gauge(G) ->
	Ft = trunc(G) div 12,
	In = trunc(G) rem 12,
	Fr = to_fraction(G - trunc(G)),
	integer_to_list(Ft) ++ "-" ++ integer_to_list(In) ++ " " ++ Fr.

is_valid_gauge(Ft, In, Fr) when is_integer(Ft), is_integer(In), is_number(Fr) ->
    is_valid_gauge1(Ft, In, Fr).

is_valid_gauge1(Ft, In, Fr) when Ft >= 0, In >= 0, In < 12, Fr >= 0, Fr =< 0.9375 ->
	is_valid_fraction(Fr);

is_valid_gauge1(_, _, _) ->
    false.

is_valid_fraction(F) -> 
	case is_integral(F, ?SIXTEENTH) of
		true ->
			true;
		_-> 
			false
	end.

delta(G1, G2) when is_float(G1), is_float(G2)->
	G1-G2;
delta(G1, G2) when is_tuple(G1), is_tuple(G2)->
	to_pretty_gauge(to_decimal(G1) - to_decimal(G2)).

to_fraction(X) when X < 0 -> [];
to_fraction(0)  	-> [];
to_fraction(0.0)  	-> [];
to_fraction(0.0625) -> "1/16";
to_fraction(0.125)  -> "1/8";
to_fraction(0.1875) -> "3/16";
to_fraction(0.25)  	-> "1/4";
to_fraction(0.3125) -> "5/16";
to_fraction(0.375)  -> "3/8";
to_fraction(0.4375) -> "7/16";
to_fraction(0.5)  	-> "1/2";
to_fraction(0.5625) -> "9/16";
to_fraction(0.625) 	-> "5/8";
to_fraction(0.6875) -> "11/16";
to_fraction(0.75)  	-> "3/4";
to_fraction(0.8125) -> "13/16";
to_fraction(0.875) 	-> "7/8";
to_fraction(0.9375) -> "15/16".

%%----------------------------------------------------------------------
%% Misc. Utils
%%----------------------------------------------------------------------

weighted_avg({A, B},{C, D}) ->
	(roundit(A/(A + C), 3) * B) + (roundit(C/(A + C), 3) * D).

roundit(N, Pr) ->
  	P=math:pow(10, Pr),
 	round(N * P) / P.

is_integral(N, D) ->
	(N / D - trunc(N / D)) == 0.0.

interpolate(L, H, V) ->
  	(V-trunc(V)) * (H - L) + L.

to_integer(L) when is_list(L) ->
	  I = (catch erlang:list_to_integer(L)),
	case is_number(I) of
	  	true ->
      		I;
		false ->
      		{error, 'not_an_integer'}
	end.

to_float(L) when is_list(L) ->
  	F = (catch erlang:list_to_float(L)),
  	case is_number(F) of
	  	true ->
      		F;
    	false ->
			I = to_integer(L),
			case is_number(I) of
				true ->
					I + 0.0;
				false ->
      				{error, 'not_a_float'}
			end
 	 end.

bjoin(List) ->
    F = fun(A, B) -> <<A/binary, B/binary>> end,
    lists:foldr(F, <<>>, List).

format_datetime(Dt) ->
	{{Y,Mo,D},{H,M,S}} = calendar:now_to_local_time(Dt),
	Date = integer_to_list(Mo) ++ "-" ++ integer_to_list(D) ++ "-" ++ integer_to_list(Y),
	Time = integer_to_list(H) ++ ":" ++ integer_to_list(M) ++ ":" ++ integer_to_list(S),
	Date ++ " " ++ Time ++ " EST".

get_arg(A, Key) -> 
	L=yaws_api:parse_post(A),
  	case lists:keyfind(Key, 1, L) of
  	{_,Value} -> 
		Value;
    _ -> 
		undefined
  end.

setup_args(A) ->
    {ok, Sess, _} = check_cookie(A),
	#gauge{
		id = now(),
		gauge = parse_gauge(get_arg(A, "mobiscroll")),
		gauger = Sess#sess.user,
		hatch = list_to_atom(get_arg(A, "hatch")),
		tank = to_integer(get_arg(A, "tank")),
		temp = to_float(get_arg(A, "temp")),
		ambient = to_integer(get_arg(A, "ambient")),
		grav = to_float(get_arg(A, "grav")),
		type = list_to_atom(get_arg(A, "type")),
		table = list_to_atom(get_arg(A, "table"))
	}.

%%----------------------------------------------------------------------
%% Authentication/Verification
%%----------------------------------------------------------------------

top(A) ->
    case check_cookie(A) of
		{ok, _Session, _Cookie} ->
		    ok;
		{error, _Reason} ->
		    lbody("Please Login") 
    end.

check_cookie(A) ->
    H = A#arg.headers,
    case yaws_api:find_cookie_val("ssid", H#headers.cookie) of
		Val when Val /= [] ->
		    case yaws_api:cookieval_to_opaque(Val) of
		        {ok, Sess} ->
		            {ok, Sess, Val};
		        {error, {has_session, Sess}} ->
		            {ok, Sess};
		        Else ->
		            Else
		    end;
		[] ->
		   {error, nocookie}
    end.

logout(A) ->
	{ok, _Sess, Cookie} = check_cookie(A),
  	yaws_api:delete_cookie_session(Cookie),
  	lbody([]).

check_auth_test(User, Pwd) ->
	case lists:keyfind(User, 1, [{"user","password"}]) of
	{_,Value} -> 
		case Pwd =:= Value of
			true -> 
				io:format("do_login ok ~n"),
				true;
			_ -> 
				io:format("do_login bad ~n"),
				{error, "Invalid Password"}				
		end;
	_ -> 
		{error, "Invalid User"}		
  end.

check_auth(User, Pwd) ->
	case get_user(User) of
		{ok, _} -> 
		case check_passwd(User, Pwd) of
			ok -> 
				true;
			_ -> 
				{error, "User and password do not match."}				
		end;
		_ -> 
			{error, "User and password do not match."}		
  	end.

check_passwd(User,Passwd) -> 
    case get_passwd(User) of
		{ok,Passwd}    -> ok;
		{ok,_}         -> {error,wrong_password};
		{error,Reason} -> {error,Reason}
    end.

get_passwd(User) ->
    case get_user(User) of
		{ok,U}         -> {ok,U#user.passwd};
		{error,Reason} -> {error,Reason}
    end.

get_user(User) ->
    F = fun() -> [U] = mnesia:read({user, User}), U end,
    case mnesia:transaction(F) of
			{atomic,U} -> {ok,U};
		_ -> 
			{error, user_not_found}
    end.


get_options(Z1, Z2) -> 
	get_options(Z1, Z2, []).

get_options(Z1, [H|T], Acc) ->
	L = case H =:= Z1 of
		true ->
			option({H, true});
		false ->
			option({H})
	end,
	get_options(Z1, T, Acc ++ [L]);

get_options(_, [], Acc) -> Acc.	

%%----------------------------------------------------------------------
%% Mobiscroll Initializer
%%----------------------------------------------------------------------

to_array(X) ->
   	to_array(X, []).

to_array([H|T], A) ->
	E = case T of
		[] ->
			"\"";
		_ ->
			"\","
	end,
	B = "\"" ++ integer_to_list(H) ++ E,
	to_array(T, A ++ B);

to_array([], A) ->
	list_to_binary("[" ++ A ++ "]").

function_mobi_tank(X)->
	mobi_tank(to_array(X)).	

%%----------------------------------------------------------------------
%% Login 
%%----------------------------------------------------------------------

login(A) ->
	User = get_arg(A, "user"),
	Password = get_arg(A, "password"),
	%%Pwd = erlang:md5(get_arg(A, "password")),
	%%case check_auth_test(User, Pwd) of
	case check_auth(User, Password) of 
		true ->
	  		io:format("User ~p logged in ", [User]),
	  		Sess = #sess{user = User, passwd = Password},
	  		Cookie = yaws_api:new_cookie_session(Sess),
	  		[yaws_api:redirect("index.yaws"),
	   		yaws_api:setcookie("ssid",Cookie)];
		{error, Why}->
			io:format("Login error: ~p", [Why]),
			io:format("User: ~p", [User]),
			lbody(Why)
	end.

%%----------------------------------------------------------------------
%% Message Dialogs 
%%----------------------------------------------------------------------

alert(Alert, Message) ->
	{ehtml,
		{table, [{align, "center"}],
			[
				{tr, [{align, "center"}], 
					[
						{td, [], [{h2, [], [Alert]}]}
					]
				},
				{tr, [{align, "center"}], 
					[
						{td, [], [{br, [], []}, Message]}						
					]
				},
				{tr, [{align, "center"}], 
					[
						{td, [], [{br, [], []}]}						
					]
				},
				{tr, [{align, "center"}], 
					[
						{td, [], [{a, [{href, "gauge.yaws"}, {'data-role', "button"}], ["Close"]}]}
					]
				}
			]
		}
	}.

%%----------------------------------------------------------------------
%% Main App UI
%%----------------------------------------------------------------------

about(_A) ->
	[header(), toolbar(), alert("About...", "Just a little app.<br/> Copyright (C), Wally Cash, All rights reserved."), footer()].


gauge_to_mobile(A) ->
	X1=string:tokens(get_arg(A, "mobiscroll"), " "),
	{T, Action} = {to_integer(nth(1,X1)), list_to_atom(nth(2,X1))},
	[Tk]=tank:get_tank(T),
	case Action of 
		info ->
			[header(), toolbar(), tkinfo_body(Tk), footer()];
		edit ->
			[header(), toolbar(), edit_tk_body(T), footer()];
		previous ->
			[G] = db:select_last_gauge(T),
			B = case G of 
				[] ->
					alert("Error!", "There is no previous gauge for this tank");
				_ ->
					gauge_result_body(G)
			end,
			[header_gauge(), toolbar(), B, footer()];
		_ ->
			case Tk#tank.center of
				undefined ->
					C = [],
					D = type_only(),
					{C, D};
				_ ->
					C = [
						{td, [], ["&nbsp;&nbsp;&nbsp;&nbsp"]},
						{td, [], [{h4, [], ["Center: " ++ to_pretty_gauge(Tk#tank.center)]}]}
					],
					D = type_and_hatch(),
					{C, D}
			end,
			E = case Action of
				gauge ->
					[];
				stopout ->
					amount();
				stopin ->
					[temp2(), grav2(), amount()];
				_ ->
					[]
			end,
			I = case Tk#tank.insulated of
				true ->
					input("hidden","ambient", [{value, 60}]);
				_ ->
					ambient()
			end,
			Body =
				{ehtml,  
					[   
						{table, [{align, "center"}],
							[
								{tr, [{align, "center"}], 
									[
										{td, [], [{h4, [], ["Tk: " ++ integer_to_list(Tk#tank.id)]}]},
										{td, [], ["&nbsp;&nbsp;&nbsp;&nbsp"]},
										{td, [], [{h4, [], ["Side: " ++ to_pretty_gauge(Tk#tank.side)]}]},
										C					
									]
								}

							]
						},
						{form, 
							[{method, "post"}, {'data-ajax', "false"}, {action, "gtpost.yaws"}],
							[
								mobi(),
								temp(),
								I,
								D,
								E,
								input("hidden","tank", [{value, Tk#tank.id}]),
								input("hidden","action", [{value, Action}]),
								input("hidden","grav", [{value, io_lib:format("~.1f", [Tk#tank.grav])}]),
								input("hidden","table", [{value, Tk#tank.table}]),
								submit()
							] 
						} 
					] 
				},
			[header_gauge(), toolbar(), Body, footer()]
	end.

show_gauge_result(A) -> 
	Action = list_to_atom(get_arg(A, "action")),
	case Action of 
		gauge ->
			[header_gauge(), toolbar(), calc_gauge(setup_args(A)), footer()];
		stopout ->
			[header_gauge(), toolbar(), calc_stop(A), footer()];
		stopin ->
			[header_gauge(), toolbar(), calc_stop(A), footer()];
		 _ ->
			[]
	end.

tkinfo_body(X) ->
	C = case X#tank.center of
		undefined ->
			[];
		_ ->
			[
				{p, [], ["Center Gauge Height: ", to_pretty_gauge(X#tank.center)]}
			]
	end,

	{ehtml,
		[
			{'div', [{class, "ui-body ui-body-d"}], 
				[
					{h3, [], "Tank Information"},
					{p, [], ["Tank: ", integer_to_list(X#tank.id)]},
					{p, [], ["Side Gauge Height: ", to_pretty_gauge(X#tank.side)]},
					C,
					{p, [], ["Safe Height: ", to_pretty_gauge(X#tank.safe)]},
					{p, [], ["Product: ", X#tank.product]},
					{p, [], ["API Gravity: ", io_lib:format("~.1f",[X#tank.grav])]}
				]	
			}
		]
	}.

gauge_result_body(G) ->
	I = case G#gauge.type of
		ullage ->
			[
				{p, [], ["Gauge Height: ", to_pretty_gauge(G#gauge.gauge_ht)]},
				{p, [], ["Innage by Ullage: ", to_pretty_gauge(G#gauge.cgauge)]}
			];
		_ ->
			[]
	end,
	{ehtml,
		[
			{'div', [{class, "ui-body ui-body-d"}], 
				[
					{h3, [], "Measurement"},
					{p, [], ["Tank: ", integer_to_list(G#gauge.tank)]},
					{p, [], ["Date/Time: ", format_datetime(G#gauge.id)]},
					{p, [], ["Gauger: ", G#gauge.gauger]},
					{p, [], ["Product: ", G#gauge.product]},
					{p, [], ["Gauge: ", to_pretty_gauge(G#gauge.gauge)]},
					{p, [], ["Hatch: ", atom_to_list(G#gauge.hatch), "&nbsp;&nbsp;&nbsp;&nbsp;", "Type: ", atom_to_list(G#gauge.type)]},
					{p, [], ["Temp: ", io_lib:format("~.1f",[G#gauge.temp]), "&nbsp;&nbsp;&nbsp;&nbsp;", "API Gravity: ", io_lib:format("~.1f",[G#gauge.grav])]},
					I
				]
			},
			{br, [], []},
			{'div', [{class, "ui-body ui-body-e"}],
				[
					{table, [{'data-role',"reflow"},{class,"ui-responsive table-stroke"}],
						[		 							
							{h3, [], "Result"},
							{thead, [], 
								[
									factors(G)
								]
							}
						]
					},
					{table, [{'data-role',"table"},{class,"ui-responsive table-stroke"}],
						[
							{thead, [], 
								[
									{tr, [], 
										[
											{th, [{'data-priority',"1"}], [""]},
											{th, [{'data-priority',"2"}], ["Gallons"]},
											{th, [{'data-priority',"3"}], ["Barrels"]}
										]
									}
								]
							},
							{tbody, [], 
								[
									{tr, [], 
										[
											{th, [], ["Gross"]},
											{td, [], [integer_to_list(G#gauge.gross)]},
											{td, [], [io_lib:format("~.2f",[G#gauge.gross / 42])]}
										]
									},
									{tr, [], 
										[
											{th, [], ["Gross*SCF"]},
											{td, [], [integer_to_list(G#gauge.gross_x_scf)]},
											{td, [], [io_lib:format("~.2f",[G#gauge.gross_x_scf / 42])]}
										]
									},
									{tr, [], 
										[
											{th, [], ["Net"]},
											{td, [], [integer_to_list(G#gauge.net)]},
											{td, [], [io_lib:format("~.2f",[G#gauge.net / 42])]}
										]
									},
									{tr, [], 
										[
											{th, [], ["Space"]},
											{td, [], [integer_to_list(G#gauge.aspace)]},
											{td, [], [io_lib:format("~.2f",[G#gauge.aspace / 42])]}
										]
									}
								]
							}
						]
					}		
				]
			}
		]
	}.

index(_A) ->
Body =
	{ehtml,
		[
			{'div', [{class, "choice_list"}], []},
			{ul, [{'data-role', "listview"}, {'data-inset', "true"}],
				[
					{'li', [],
						[{a, [{href, "gauge.yaws"}, {'data-transition', "slidedown"}], ["Tank Gauging"]}] 
					}, 
					{'li', [],
						[{a, [{href, "about.yaws"}, {'data-transition', "slidedown"}], ["About"]}] 
					}
				] 
			} 
		] 
	},
	[header(), toolbar(), Body, footer()].

lbody(M) ->
Body =
	{ehtml,
		[	
			{h3, [], M}, 
			{form, [{method, "post"}, {'data-ajax', "false"}, {action, "lpost.yaws"}],
				[
					user(),
					password(),
					button("submit", "submit", [{value, "Submit"}, {'data-inline', "true"}], ["Login"])
				] 
			} 
		] 
	},
	[header(), toolbar_mt(), Body, footer()].

gauge(_A) ->
Body =
	{ehtml,
		[
			{form, [{method, "post"}, {'data-ajax', "false"}, {action, "gpost.yaws"}],
				[	{p, [{align, "center"}], ["Scroll to select a tank and an action."]}, 	
					mobi(),
					submit()
				] 
			} 
		] 
	},
	[header_tk(), toolbar(), Body, footer()].

edit_tk_body(T) ->
	[G]=db:select_tank(T),
	X= get_options(G#tank.product, db:select_products()),
	Body = {ehtml, 
		[
			{form, 
				[{method, "post"}, {'data-ajax', "false"}, {action, "etpost.yaws"}],
				[
					{h3, [], "Edit Tank Information"},
					{p, [], ["Tank: ", integer_to_list(T)]},
					flip_switch(),
					select(product, "Product: ", [], X),
					input("range", "grav", "API Gravity: ", [{min, "0"}, {'data-highlight', "true"}, {step, "0.1"}, {max, "40"}, {value, io_lib:format("~.1f", [G#tank.grav])}]),
					input("hidden","tank", [{value, T}]),	
					submit()
				] 
			}
	 	]
	},
	Body.

flip_switch() ->
	X = get_options("off", ["on", "off"]),
	select(switch, "Switch: ", [{'data-role', "slider"}], X). 

%%----------------------------------------------------------------------
%% Page Fragments
%%----------------------------------------------------------------------

type_and_hatch() ->
	{table, [{align, "center"}],
		[
			{tr, [{align, "center"}], 
				[
					{td, [], [type()]},
					{td, [], ["&nbsp;&nbsp;&nbsp;&nbsp"]},
					{td, [], [hatch()]}
				]
			}

		]
	}.

type_only() ->
	{table, [{align, "center"}],
		[
			{tr, [{align, "center"}], 
				[
					{td, [], [type()]},
					input("hidden","hatch", [{value, "side"}])
				]
			}

		]
	}.

factors(G) ->
	T = factor:factor_to_text(G#gauge.table),
	[{p, [], ["Table: ", T]}, 
	{p, [], ["VCF: ", io_lib:format("~.4f",[G#gauge.vcf])]},
	{p, [], ["SCF: ", io_lib:format("~.5f",[G#gauge.scf])]}].

sbody({G, Stop, Net, Ntgal}) ->
	{ehtml,
		[
			{'div', [{class, "ui-body ui-body-d"}], 
				[
					{h3, [], "Tank"},
					{p, [], ["Tank: ", integer_to_list(G#gauge.tank)]},
					{p, [], ["Gauge: ", to_pretty_gauge(G#gauge.gauge)]},
					{p, [], ["Temp: ", io_lib:format("~.1f",[G#gauge.temp]), "&nbsp;&nbsp;&nbsp;&nbsp;", "API Gravity: ", io_lib:format("~.1f",[G#gauge.grav])]},
					factors(G)
				]
			},
			{br, [], []},
			{'div', [{class, "ui-body ui-body-e"}], 
				[
					{h3, [], "Target"},
					{p, [], ["Net Barrels: ", integer_to_list(trunc(Ntgal / ?BBL))]},
					{p, [], ["Net Gallons: ", integer_to_list(Ntgal)]}
				]
			},
			{br, [], []},
			{'div', [{class, "ui-body ui-body-c"}], 
				[
					{h3, [], "Actual Stop"},
					{p, [], ["Gauge: ", to_pretty_gauge(Stop)]},
					{p, [], ["Net Barrels: ", io_lib:format("~.2f", [abs(Net / ?BBL)])]},			
					{p, [], ["Net Gallons: ", integer_to_list(abs(Net))]}
				]
			}
		]
	};

sbody({G, Stop, Net, Ntgal, Temp2, Grav2, Vcf, Temp3, Grav3}) ->
	{ehtml,
		[
			{'div', [{class, "ui-body ui-body-d"}], 
				[
					{h3, [], "Tank"},
					{p, [], ["Tank: ", integer_to_list(G#gauge.tank)]},
					{p, [], ["Gauge: ", to_pretty_gauge(G#gauge.gauge)]},
					{p, [], ["Temp: ", io_lib:format("~.1f",[G#gauge.temp]), "&nbsp;&nbsp;&nbsp;&nbsp;", "API Gravity: ", io_lib:format("~.1f",[G#gauge.grav])]},
					{table, [{'data-role',"reflow"},{class,"ui-responsive table-stroke"}, {cellpadding, "7"}]}
				]
			},
			{br, [], []},
			{'div', [{class, "ui-body ui-body-e"}], 
				[
					{h3, [], "Target"},
					{p, [], ["Temp: ", io_lib:format("~.1f", [Temp2]), "&nbsp;&nbsp;&nbsp;&nbsp;", "API Gravity: ", io_lib:format("~.1f",[Grav2])]},
					{p, [], ["Vcf: ", io_lib:format("~.4f", [Vcf])]},
					{p, [], ["Net Barrels: ", integer_to_list(trunc(Ntgal / ?BBL))]},
					{p, [], ["Net Gallons: ", integer_to_list(Ntgal)]}
				]
			},
			{br, [], []},
			{'div', [{class, "ui-body ui-body-c"}], 
				[
					{h3, [], "Actual Stop"},
					{p, [], ["Gauge: ", to_pretty_gauge(Stop)]},
					{p, [], ["Net Barrels: ", io_lib:format("~.2f", [abs(Net / ?BBL)])]},			
					{p, [], ["Net Gallons: ", integer_to_list(abs(Net))]}, 
					{p, [], ["Calc. Temp: ", io_lib:format("~.1f", [Temp3]), "&nbsp;&nbsp;&nbsp;&nbsp;", "Calc. Gravity: ", io_lib:format("~.1f",[Grav3])]}
				]
			}
		]
	}.

head(I,F) ->

	Open = [<<"
	<html>
	<head>
	<meta charset=\"utf-8\">
	<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
	<title></title>
	">>],
	Close= [<<"
	</head>
	<body>
	<div id=\"gauge\" data-role=\"page\">
	">>],
 	{html, bjoin(Open ++ I ++ [<<"<script>">>] ++ F ++ [<<"</script>">>] ++ Close)}.

toolbar() ->
	{html, [<<"
	<div data-role=\"header\" data-theme=\"b\">
 	<div class=\"ui-btn-left\" data-role=\"controlgroup\" data-type=\"horizontal\">
	<a href=\"index.yaws\" data-role=\"button\" data-icon=\"home\">Done</a>
  	</div>
	<h1></h1>
 	<div class=\"ui-btn-right\" data-role=\"controlgroup\" data-type=\"horizontal\">
	<a href=\"logout.yaws\" data-role=\"button\">Logout</a>
	</div>
	</div> 
	<div data-role=\"content\">
	">>]}.

toolbar_mt() ->
	{html, [<<"
	<div data-role=\"header\" data-theme=\"b\">
	<h1></h1>
	</div> 
	<div data-role=\"content\">
	">>]}.

footer() -> 
	{html, [<<"
	</div></div></body></html>
	">>]}.

%%----------------------------------------------------------------------
%% Head Fragments
%%----------------------------------------------------------------------

include_jq_mobi() ->
 	<<"
    <script type=\"text/javascript\" src=\"http://cdn.app-framework-software.intel.com/1.2/jq.mobi.min.js\"></script>
	">>.

include_custom() ->
 	<<"
	<link rel=\"shortcut icon\" href=\"favicon.ico\">
	<link rel=\"stylesheet\" href=\"http://fonts.googleapis.com/css?family=Open+Sans:300,400,700\">
    <link href=\"css/custom.css\" rel=\"stylesheet\" type=\"text/css\" />
	">>.


include_jqm() ->
 	<<"
    <link rel=\"stylesheet\" href=\"http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.css\" />
    <script src=\"http://code.jquery.com/jquery-1.9.1.min.js\"></script>
    <script src=\"http://code.jquery.com/mobile/1.3.2/jquery.mobile-1.3.2.min.js\"></script>
	">>.

include_mobi() ->
 	<<"
    <script src=\"js/mobiscroll.core.js\"></script>
    <script src=\"js/mobiscroll.scroller.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.select.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.jqm.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.ios.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.ios7.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.android.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.android-ics.js\" type=\"text/javascript\"></script>
    <script src=\"js/mobiscroll.scroller.wp.js\" type=\"text/javascript\"></script>
    <link href=\"css/mobiscroll.scroller.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link href=\"css/mobiscroll.scroller.jqm.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link href=\"css/mobiscroll.scroller.ios7.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link href=\"css/mobiscroll.animation.css\" rel=\"stylesheet\" type=\"text/css\" />
    <link href=\"css/custom.css\" rel=\"stylesheet\" type=\"text/css\" />
	">>.

function_theme() ->
 	<<"
		$(document).on('pagecreate', function(){
			$.mobile.listview.prototype.options.headerTheme = \"a\";
			$.mobile.defaultDialogTransition = 'pop';
			$.mobile.defaultPageTransition = 'flip';
			$.mobile.selectmenu.prototype.options.nativeMenu = false;
			$.mobile.ajaxEnabled=false;
		});
	">>.

mobi_tank(K) ->

 	A = <<"
		 $(function(){
			var feet = range(0, 50);
			var inches = range(0, 11);
			$('#mobiscroll').scroller({
				wheels: [[ 
				{  label: 'Tank',
				   keys: ">>,
	B = <<" 
,  
				   values: ">>,

	C = <<"
				},
				{  
				    keys: ['gauge', 'stopout', 'stopin', 'previous', 'info', 'edit'],  
				    values: ['Enter a Gauge', 'Stop a Discharge', 'Stop a Reciept', 'Previous Gauge', 'Tank Info', 'Edit Grav/Product']
				}],],
				display: 'inline',
				theme: 'ios7',
				height: '20'
			}).scroller('setValue', '1 gauge', true);;    
		});

		function range(start, end) {
				var foo = [];
				for (var i = start; i <= end; i++) {
					  foo.push(i);
				}
				return foo;
		}
	">>,

bjoin([A, K, B, K, C]).

function_mobi_gauge() ->
 	<<"
		 $(function(){
				var feet = range(0, 50);
				var inches = range(0, 11);
			$('#mobiscroll').scroller({
				wheels: [[ 
				{ label: 'Feet', keys: feet, values: feet,}, 
				{ label: 'Inches', keys: inches, values: inches, },
				{ label: 'Fraction', 
				    keys: ['0.0','0.0625','0.125','0.1875','0.25','0.3125','0.375','0.4375','0.5','0.5625','0.625','0.6875','0.75','0.8125','0.875','0.9375'],  
				    values: ['none','1/16','1/8','3/16','1/4','5/16','3/8','7/16','1/2','9/16','5/8','11/16','3/4','13/16','7/8','15/16']
				}],],
				display: 'inline',
				theme: 'jqm',
				height: '20'
			});    
		});

		function range(start, end) {
				var foo = [];
				for (var i = start; i <= end; i++) {
					  foo.push(i);
				}
				return foo;
		}
	">>.	

%%----------------------------------------------------------------------
%% Component Wrappers 
%%----------------------------------------------------------------------

header_mobi() -> header.
header_mobi1() -> header.
header() -> head([include_jqm(), include_custom()],[function_theme()]).
header_tk() -> head([include_jq_mobi(), include_jqm(), include_mobi(), include_custom()],[function_theme(), function_mobi_tank(lists:sort(db:select_tk_ids()))]).
header_gauge() -> head([include_jq_mobi(), include_jqm(), include_mobi(), include_custom()],[function_theme(), function_mobi_gauge()]).
table() -> select("table", [{'data-mini', "true"}, {'data-inline', "true"}, {'data-role', "slider"}], [{"Seven","seven"},{"Six B","sixb"}]).
submit() -> button("submit", "submit", [{value, "Submit"}, {'data-inline', "true"}], ["Submit"]).
hatch() -> select("hatch", [{'data-mini', "true"}, {'data-inline', "true"}, {'data-role', "slider"}], [{"Side","side"},{"Center","center"}]).
type() -> select("type", [{'data-mini', "true"}, {'data-inline', "true"}, {'data-role', "slider"}], [{"Innage","innage"},{"Ullage","ullage"}]).
temp() -> input("range", "temp", "Temp: ", [{min, "0"}, {'data-highlight', "true"}, {'data-mini', "true"}, {step, "1"}, {min, "0"}, {max, "350"}, {value, "60.0"}]).
temp2() -> input("range", "temp2", "Cargo Temp: ", [{min, "0"}, {'data-highlight', "true"}, {'data-mini', "true"}, {step, "1"}, {min, "0"}, {max, "350"}, {value, "60.0"}]).
ambient() -> input("range", "ambient", "Ambient Temp: ", [{min, "0"}, {'data-highlight', "true"}, {step, "1"}, {min, "0"}, {max, "130"}, {value, "60"}]).
grav() -> input("range", "grav", "API Gravity: ", [{min, "0"}, {'data-highlight', "true"}, {step, "0.1"}, {max, "40"}]).
grav2() -> input("range", "grav2", "Cargo Gravity: ", [{min, "0"}, {'data-highlight', "true"}, {step, "0.1"}, {max, "40"}, {value, "5.0"}]).
gross() ->	input("number", "gross", "Gross: ", [{min, "0"}, {value, "0"}, {'data-mini',true}]).
net() ->	input("number", "net", "Net: ", [{min, "0"}, {value, "0"}, {'data-mini',true}]).
amount() -> input("number", "amount", "Target (Net BBLs): ", [{min, "0"}, {'data-mini',"true"}, {value, "0"}]).
user() -> input("text", "user", "User: ", [{min, "0"}, {'data-clear-btn', "false"}]).
password() -> input("password", "password", "Password: ", [{min, "0"}, {'data-clear-btn', "false"}]).

%%----------------------------------------------------------------------
%% JQM Component Wrappers 
%%----------------------------------------------------------------------

input(Type, Name, Label, L) ->
	{'div', [{'data-role', "fieldcontain"}],
		[
			{label, [{for, Name}], [Label]},
			{input, [{type, Type}, {name, Name}, {id, Name}] ++ [{_,_}=A||A<-L], []}
		] 
	}.

input(Type, Name, L) ->
	{input, [{type, Type}, {name, Name}, {id, Name}] ++ [{_,_}=A||A<-L], []}.

button(Type, Name, L, Txt) ->
	{p, [{align, "center"}],
		[
			{button, [{type, Type}, {name, Name}, {id, Name}] ++ [{_,_}=A||A<-L], [Txt]}
		] 
	}.

option({K}) ->{option, [], [K]};

option({K, true}) -> {option, [{selected,"selected"}], [K]};

option({K, V}) -> {option, [{value, V}], [K]};

option({K, V, true}) -> {option, [{value, V}, {selected,"selected"}], [K]}.

select(Name, L, X) ->
	{'div', [{'data-role', "fieldcontain"}],
		[
			{select, [{ name, Name}, {id, Name}] ++ [{_,_}=A||A<-L], [option(A)||A<-X]}
		] 
	}.

select(Name, Label, L, X) ->
	{'div', [{'data-role', "fieldcontain"}],
		[{label, [{for, Name}], [Label]},
		{select, [{ name, Name}, {id, Name}] ++ [{_,_}=A||A<-L], X}
		] 
	}.

%%----------------------------------------------------------------------
%% Mobiscroll Wrapper
%%----------------------------------------------------------------------

mobi() ->	
     {'div', [{'data-role', "fieldcontain"}],
		[			
			{p, [{align, "center"}],
				[
					{input, [{name, "mobiscroll"}, {id, "mobiscroll"}, { type, "hidden"}, {'data-mini',"true"}], []}
				]
			}
		]
	}.


