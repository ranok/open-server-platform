%% @author Jacob Ian Torrey <torreyji@clarkson.edu>
%% @copyright 2008 Jacob Torrey
%% @version 0.2
%% @doc An OSP servlet
-module(osp_admin).

% Export OSP server callback
-export([start_mnesia/0, server/1, init/0, cleanup/0, proto/0]).

% Export the admin functions for the web console
-export([shutdown_osp/0, stats_osp/0, uptime_osp/0]).

-include("../include/conf.hrl").

% Import the OSP socket library
-import(osp_socket, [send/2, recv/2, sendf/3, close/1]).

% Define the Mnesia record
-record(osp_table, {key, val}).

%% @doc Stores a value in the mnesia database
store(Key, Val) ->
    osp_mnesia:store(osp_admin_table, Key, Val).

%% @doc Gets a value from the mnesia database
retrieve(Key) ->
    osp_mnesia:retrieve(osp_admin_table, Key).

%% @doc Returns the proto
proto() ->
    tcp.

%% @doc The mnesia startup routine
start_mnesia() ->
    case lists:member(mnesia_sup, erlang:registered()) of
	true ->
	    ok;
	false ->
	    mnesia:start()
    end,
    case catch(mnesia:table_info(osp_admin_table, all)) of
	{'EXIT', _} ->
	    mnesia:create_table(osp_admin_table, [{record_name, osp_table}, {disc_copies, [node()]}, {attributes, record_info(fields, osp_table)}]);
	_ ->
	    ok
    end,
    ok.

%% @doc The main server loop
server(Sock) ->
    {tcp, S} = Sock,
    RH = inet:peername(S),
    case RH of
	{ok, {{127, 0, 0, 1}, _}} ->
	    send(Sock, <<"-> ">>),
	    handlecommand(Sock, recv(Sock, 0)),
	    server(Sock);
	_ ->
	    close(Sock)
    end.

handlecommand(Sock, Msg) ->
    String = erlang:binary_to_list(Msg),
    String2 = string:strip(String, right, $\n),
    String3 = string:strip(String2, right, $\r),
    case String3 of
	"help" -> % Print out the help message
	    send(Sock, <<"OSP Admin Console\n\tquit - Quits the console\n\tstats - Prints general stats about the OSP cluster\n\tstart <appname> <port> <node> - Starts appname on node\n\tadd-diskless-ip <ip> - Adds IP to the allowed diskless server pool\n\tshutdown - Shutdown OSP on all nodes\n\tadd-backup-server <node> <type> - Makes node a backup server keeping an up-to-date copy of all the shared state in the cluster; type may be ram for faster, non-persistant storage, or disk for data persistance\n\tstop <appname> <node> - Stops the given servlet on node\n\tmigrate <appname> <fromnode> <tonode> <port> - Migrates a servlet from fromnode to tonode, starting it on the given port\n">>);
	"stats" -> % Display some stats
	    send(Sock, stats_osp());
	"quit" ->
	    close(Sock),
	    exit(normal);
   	"shutdown" ->
	    send(Sock, "Shutting down\r\n"),
	    close(Sock),
	    shutdown_osp();
	"" ->
	    ok;
	Unknown ->
	    Split = string:tokens(Unknown, " "),
	    [Comm | _] = Split,
	    case Comm of
		"start" ->
		    [Comm, AppList, PortList, NodeList ] = Split,
		    Port = erlang:list_to_integer(PortList),
		    App = erlang:list_to_atom(AppList),
		    Node = erlang:list_to_atom(NodeList),
		    start_servlet(App, Port, Node, Sock);
		"migrate" ->
		    [Comm, AppList, FromNodeList, NodeList, PortList ] = Split,
		    Port = erlang:list_to_integer(PortList),
		    From = erlang:list_to_atom(FromNodeList),
		    App = erlang:list_to_atom(AppList),
		    Node = erlang:list_to_atom(NodeList),
		    start_servlet(App, Port, Node, Sock),
		    stop_servlet(App, From, Sock);
		"add-diskless-ip" ->
		    [Comm, IPList] = Split,
		    IP = erlang:list_to_atom(IPList),
		    erl_boot_server:add_slave(IP),
		    sendf(Sock, "Added ~p to the allowed diskless server pool~n", [IP]);
		"add-backup-server" ->
		    [Comm, NodeL, TypeL] = Split,
		    Node = erlang:list_to_atom(NodeL),
		    Type = erlang:list_to_atom(TypeL),
		    rpc:call(Node, mnesia, start, []),
		    case Type of
			ram ->
			    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
			    bkup_db(Node, ram_copies);
			disk ->
			    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
			    rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies]),
			    bkup_db(Node, disc_copies);
			_ ->
			    send(Sock, "Sorry, unknown backup type: " ++ TypeL ++ "\r\n")
		    end;
		"stop" ->
		    [Comm, AppL, NodeL] = Split,
		    App = erlang:list_to_atom(AppL),
		    Node = erlang:list_to_atom(NodeL),
		    stop_servlet(App, Node, Sock);
		_ ->
		    send(Sock, "Sorry, unknown command " ++ Unknown ++ "\r\n")
	    end
    end.

%% @todo This needs to update on every start of a new application
bkup_db(Node, Type) ->
    Tables = mnesia:system_info(tables),
    F = fun(TableName) ->
		mnesia:add_table_copy(TableName, Node, Type),
		rpc:call(Node, mnesia, wait_for_tables, [[TableName], 1000])
	end,
    lists:foreach(F, Tables),
    ok.

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

ip_to_string({A, B, C, D}) ->
    erlang:integer_to_list(A) ++ "." ++ erlang:integer_to_list(B) ++ "." ++ erlang:integer_to_list(C) ++ "." ++ erlang:integer_to_list(D).

get_ok_slaves() ->
    Slaves = erl_boot_server:which_slaves(),
    Fun = fun({NM, IP}, A) ->
		  A ++ "\tIP: " ++ ip_to_string(IP) ++ " Netmask: " ++ ip_to_string(NM) ++ "\r\n"
	  end,
    lists:foldl(Fun, [], Slaves).

stats_osp() ->
    F = fun(Node, A) -> A ++ io_lib:format("~p: ~.2f\r\n", [Node, round(100 * rpc:call(Node, cpu_sup, avg1, []) / 256) / 100]) end,
    Out1 = "Nodes in the cluster and their CPU Utilization: \r\n",
    Out2 = lists:foldl(F, Out1, [node() | nodes()]),
    Out2 ++ "The following IPs are allowed to be diskless:\r\n" ++ get_ok_slaves().

stop_servlet(App, Node, Sock) ->
    case lists:member(Node, [node() | nodes()]) of
	true ->
	    if
		Node =:= node() ->
		    osp_broker:stop(App);
		true->
		    rpc:call(Node, osp_broker, stop, [App])
	    end,
	    sendf(Sock, "Stopped ~p on ~p ~n", [App, Node]);
	false ->
	    send(Sock, "Sorry, the node you requested couldn't be found\r\n")
    end.

start_servlet(App, Port, Node, Sock) ->
    case lists:member(Node, [node() | nodes()]) of
	true ->
	    if
		Node =:= node() ->
		    osp_broker:start(App, Port);
		true->
		    start_db(Node, App),
		    rpc:call(Node, osp_broker, start, [App, Port])
	    end,
	    sendf(Sock, "~p started on ~p port ~p~n", [App, Node, Port]);
	false ->
	    send(Sock, "Sorry, the node you requested couldn't be found\r\n")
    end.

start_servlet(App, Port, Node) ->
    case lists:member(Node, [node() | nodes()]) of
	true ->
	    if
		Node =:= node() ->
		    osp_broker:start(App, Port);
		true->
		    start_db(Node, App),
		    rpc:call(Node, osp_broker, start, [App, Port])
	    end,
	    ok;
	false ->
	    error
    end.

uptime_osp() ->
    Seconds = retrieve(uptime),
    NSeconds = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    {{_, _, Days}, {Hours, Mins, Secs}} = calendar:gregorian_seconds_to_datetime(NSeconds - Seconds),
    erlang:integer_to_list(Days - 1) ++ " days " ++ erlang:integer_to_list(Hours) ++ " hrs " ++ erlang:integer_to_list(Mins) ++ " mins " ++ erlang:integer_to_list(Secs) ++ " secs".

shutdown_osp() ->
    F = fun(Node) ->
		rpc:call(Node, init, stop, [])
	end,
    lists:foreach(F, nodes()),
    init:stop().

init() ->
    erl_boot_server:start(['127.0.0.1']),
    DL = fun(IP) ->
		 erl_boot_server:add_slave(IP)
	 end,
    lists:foreach(DL, ?ALLOWED_DISKLESS),
    F = fun({App, Port}) ->
		start_servlet(App, Port, node())
	end,
    lists:foreach(F, ?AUTO_STARTED),
    store(uptime, calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    ok.

cleanup() ->
    ok.
  
