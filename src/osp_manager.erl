%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey
%% @doc Manages all application servlets running on the node
-module(osp_manager).

% Application callbacks
-export([startup/0, stop_node/0]).

% Export the admin functions
-export([shutdown_osp/0, stats/0, uptime/0, nodeapp/0, start_servlet/3, stop_servlet/2, bkup_db/2]).

% Define the Mnesia record
-record(osp_table, {key, val}).

%% @doc Starts the local OSP node and all applications meant to be run on the node
%% @spec startup() -> {ok, pid(), pid()}
startup() ->
    erl_boot_server:start(['127.0.0.1']),
    DL = fun(IP) ->
		 erl_boot_server:add_slave(IP)
	 end,
    lists:foreach(DL, osp:get_conf('ALLOWED_DISKLESS')),
    start_mnesia(),
    case retrieve(uptime) of 
	undefined ->
	    store(uptime, calendar:datetime_to_gregorian_seconds(erlang:universaltime()));
	_ ->
	    ok
    end,
    case retrieve(nodeapp) of
	undefined ->
	    store(nodeapp, []);
	_ ->
	    ok
    end,
    F = fun({App, Port}) ->
		start_servlet(App, Port, node())
	end,
    lists:foreach(F, osp:get_conf('AUTO_STARTED')),
    Pid = spawn(fun() -> loop() end),
    {ok, Pid, osp_web:start()}.

%% @doc Stops all the applications on a given node
%% spec stop_node() -> ok
stop_node() ->
    Apps = retrieve(nodeapp),
    Node = node(),
    {value, {Node, LocalApps}} = lists:keysearch(node(), 1, Apps),
    F = fun({App, _}) ->
		osp_broker:stop(App)
	end,
    lists:foreach(F, LocalApps),
    store(nodeapp, lists:keydelete(node(), 1, Apps)),
    ok.

%% @doc An endless loop to satisfy OTP's Pid requirement
%% @spec loop() -> none()
loop() ->
    receive
	_ ->
	    loop()
    end.

%% @doc The mnesia startup routine for osp_admin
%% @spec start_mnesia() -> ok
start_mnesia() ->
    case lists:member(mnesia_sup, erlang:registered()) of
	true ->
	    ok;
	false ->
	    mnesia:start()
    end,
    case catch(mnesia:table_info(osp_table, all)) of
	{'EXIT', _} ->
	    mnesia:create_table(osp_table, [{record_name, osp_table}, {disc_copies, [node()]}, {attributes, record_info(fields, osp_table)}]);
	_ ->
	    ok
    end,
    ok.

%% @doc Stores a value in the mnesia database
%% @spec store(any(), any()) -> ok
store(Key, Val) ->
    osp_mnesia:store(osp_table, Key, Val).

%% @doc Gets a value from the mnesia database
%% @spec retrieve(any()) -> any()
retrieve(Key) ->
    osp_mnesia:retrieve(osp_table, Key).

%% @doc Returns the App-Node listing for the cluster
%% @spec nodeapp() -> list()
nodeapp() ->
    retrieve(nodeapp).

%% @todo This needs to update on every start of a new application
%% @spec bkup_db(node(), atom()) -> ok
bkup_db(Node, Type) ->
    Tables = mnesia:system_info(tables),
    F = fun(TableName) ->
		mnesia:add_table_copy(TableName, Node, Type),
		rpc:call(Node, mnesia, wait_for_tables, [[TableName], 1000])
	end,
    lists:foreach(F, Tables),
    ok.

%% @doc Starts an appropriately typed table copy for a passed 
%% @spec start_db(node(), atom()) -> ok | error
start_db(Node, App) ->
    TableName = erlang:list_to_atom(erlang:atom_to_list(App) ++ "_table"),
    rpc:call(Node, mnesia, start, []),
    case rpc:call(Node, init, get_argument, [loader]) of
	error ->
	    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
	    rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies]),
	    mnesia:add_table_copy(TableName, Node, disc_copies);
	{ok, [["inet"]]} ->
	    mnesia:add_table_copy(TableName, Node, ram_copies),
	    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]);
	{ok, [["efile"]]} ->
	    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
	    rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies]),
	    mnesia:add_table_copy(TableName, Node, disc_copies)
       end,
    rpc:call(Node, mnesia, wait_for_tables, [[TableName], 1000]),
    ok.

%% @doc Returns a string of the allowed slave IPs
%% @spec get_ok_slaves() -> string()
get_ok_slaves() ->
    Slaves = erl_boot_server:which_slaves(),
    Fun = fun({NM, IP}, A) ->
		  A ++ "\tIP: " ++ ip_to_string(IP) ++ " Netmask: " ++ ip_to_string(NM) ++ "\r\n"
	  end,
    lists:foldl(Fun, [], Slaves).

%% @doc Returns a human readable string from a tuple version of an IP address
%% @spec ip_to_string(tuple()) -> string()
ip_to_string({A, B, C, D}) ->
    erlang:integer_to_list(A) ++ "." ++ erlang:integer_to_list(B) ++ "." ++ erlang:integer_to_list(C) ++ "." ++ erlang:integer_to_list(D).

%% @doc This returns a string of the OSP cluster statistics
%% @spec stats() -> string()
stats() ->
    F = fun(Node, A) -> A ++ io_lib:format("~p: ~.2f\r\n", [Node, round(100 * rpc:call(Node, cpu_sup, avg1, []) / 256) / 100]) end,
    Out1 = "Nodes in the cluster and their CPU Utilization: \r\n",
    Out2 = lists:foldl(F, Out1, [node() | nodes()]),
    Out2 ++ "The following IPs are allowed to be diskless:\r\n" ++ get_ok_slaves().

%% @doc Attempts to find and stop an application on the cluster
%% @spec stop_servlet(atom(), node()) -> ok | error
stop_servlet(App, Node) ->
    case lists:member(Node, [node() | nodes()]) of
	true ->
	    if
		Node =:= node() ->
		    osp_broker:stop(App);
		true->
		    rpc:call(Node, osp_broker, stop, [App])
	    end,
	    del_app_from_list(Node, App),
	    ok;
	false ->
	    error
    end.

%% @doc Starts a servlet application on a given node
%% @spec start_servlet(atom(), int(), node()) -> ok | error
start_servlet(App, Port, Node) ->
    case lists:member(Node, [node() | nodes()]) of
	true ->
	    case code:load_file(App) of
		{module, App} ->
		    if
			Node =:= node() ->
			    osp_broker:start(App, Port);
			true->
			    start_db(Node, App),
			    rpc:call(Node, osp_broker, start, [App, Port])
		    end,
		    add_app_to_list(Node, App, Port),
		    ok;
		{error, _} ->
		    {error, noapp}
	    end;
	false ->
	    {error, nonode}
    end.

%% @doc Returns a human readable string of the cluster uptime
%% @spec uptime() -> string()
uptime() ->
    Seconds = retrieve(uptime),
    NSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    {{_, _, Days}, {Hours, Mins, Secs}} = calendar:gregorian_seconds_to_datetime(NSeconds - Seconds),
    erlang:integer_to_list(Days - 1) ++ " days " ++ erlang:integer_to_list(Hours) ++ " hrs " ++ erlang:integer_to_list(Mins) ++ " mins " ++ erlang:integer_to_list(Secs) ++ " secs".

%% @doc Shuts down the entire OSP cluster, and quits the Erlang VM
%% @spec shutdown_osp() -> ok
shutdown_osp() ->
    F = fun(Node) ->
		rpc:call(Node, application, stop, [osp]),
		rpc:call(Node, init, stop, [])
	end,
    lists:foreach(F, nodes()),
    store(uptime, undefined),
    store(nodeapp, []),
    init:stop().

%% @doc Deletes an application from the list of running applications
%% @spec del_app_from_list(atom(), atom()) -> ok
del_app_from_list(Node, App) ->
    NodeApp = retrieve(nodeapp),
    {Node, AppList} = lists:keyfind(Node, 1, NodeApp),
    AL2 = lists:keydelete(App, 1, AppList),
    store(nodeapp, lists:keyreplace(Node, 1, NodeApp, {Node, AL2})),
    ok.

%% @doc Adds an application to the list of running applications
%% @spec add_app_to_list(atom(), atom(), integer()) -> ok
add_app_to_list(Node, App, Port) ->
    Nodeapp = retrieve(nodeapp),
    case lists:keyfind(Node, 1, Nodeapp) of
	false ->
	    store(nodeapp, [{Node, [{App, Port}]} | Nodeapp]);
	{Node, AppList} ->
	    store(nodeapp, lists:keyreplace(Node, 1, Nodeapp, {Node, AppList ++ [{App, Port}]}))
    end,
    ok.
