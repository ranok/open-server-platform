%% @author Jacob Ian Torrey <torreyji@clarkson.edu>
%% @copyright 2008 Jacob Torrey
%% @version 0.2
%% @doc An OSP servlet
-module(osp_admin).

% Export OSP server callback
-export([start_mnesia/0, server/1, init/0, cleanup/0, proto/0]).

% Import the OSP socket library
-import(osp_socket, [send/2, recv/2, sendf/3, close/1]).

%% @doc Returns the proto
proto() ->
    tcp.

%% @doc The mnesia startup routine
start_mnesia() ->
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
	    send(Sock, <<"OSP Admin Console\n\tquit - Quits the console\n\tstats - Prints general stats about the OSP cluster\n\tstart <appname> <port> <node> - Starts appname on node\n\tadd-diskless-ip <ip> - Adds IP to the allowed diskless server pool\n\tshutdown - Shutdown OSP on all nodes\n\tadd-backup-server <node> <type> - Makes node a backup server keeping an up-to-date copy of all the shared state in the cluster; type may be ram for faster, non-persistant storage, or disk for data persistance\n">>);
	"stats" -> % Display some stats
	    F = fun(Node) -> sendf(Sock, "~p: ~f~n", [Node, round(100 * rpc:call(Node, cpu_sup, avg1, []) / 256) / 100]) end,
	    sendf(Sock, "Nodes in the cluster and their CPU Utilization: ~n", []),
	    lists:map(F, [node() | nodes()]),
	    sendf(Sock, "The following IPs: ~p are allowed to be diskless~n", [erl_boot_server:which_slaves()]);
	"quit" ->
	    close(Sock),
	    exit(normal);
   	"shutdown" ->
	    F = fun(Node) ->
			rpc:call(Node, osp_broker, shutdown, []),
			rpc:call(Node, init, stop, [])
		end,
	    lists:foreach(F, nodes()),
	    send(Sock, "Shutting down\r\n"),
	    close(Sock),
	    lists:foreach(F, nodes()),
	    osp_broker:stop(osp_admin),
	    osp_broker:shutdown(),
	    init:stop();
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
		_ ->
		    send(Sock, "Sorry, unknown command " ++ Unknown ++ "\r\n")
	    end
    end.

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

init() ->
    application:start(sasl),
    application:start(os_mon),
    mnesia:start(),
    erl_boot_server:start(['127.0.0.1']),
    AllowedNodes = osp:get_conf(allowed_diskless, "osp.conf"),
    DL = fun(IP) ->
		 erl_boot_server:add_slave(IP)
	 end,
    lists:foreach(DL, AllowedNodes),
    AutoStart = osp:get_conf(auto_started, "osp.conf"),
    F = fun({App, Port}) ->
		start_servlet(App, Port, node())
	end,
    lists:foreach(F, AutoStart),
    ok.

cleanup() ->
    application:stop(sasl),
    application:stop(os_mon),
    mnesia:stop(),
    ok.
  
