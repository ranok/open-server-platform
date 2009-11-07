%% @author Jacob Ian Torrey <torreyji@clarkson.edu>
%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @version 0.4
%% @doc Provides a telnet interface to administer the OSP cluster
-module(osp_admin).
-behavior(osp_servlet).

% Export OSP server callback
-export([start_mnesia/0, server/1, init/0, cleanup/0, proto/0]).

% Import the OSP socket library
-import(osp_socket, [send/2, recv/2, sendf/3, close/1]).

%% @doc Returns the protocol for the application (this is a TCP applcation)
%% @spec proto() -> tcp
proto() ->
    tcp.

%% @doc The mnesia startup routine for osp_admin
%% @spec start_mnesia() -> ok
start_mnesia() ->
    ok.

%% @doc The main server loop
%% @spec server(tuple()) -> none()
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

%% @doc Parses incoming command from the telnet console and performs the command if valid
handlecommand(Sock, Msg) ->
    String = erlang:binary_to_list(Msg),
    String2 = string:strip(String, right, $\n),
    String3 = string:strip(String2, right, $\r),
    case String3 of
	"help" -> % Print out the help message
	    send(Sock, <<"OSP Admin Console\n\tquit - Quits the console\n\tstats - Prints general stats about the OSP cluster\n\tstart <appname> <port> <node> - Starts appname on node\n\tadd-diskless-ip <ip> - Adds IP to the allowed diskless server pool\n\tshutdown - Shutdown OSP on all nodes\n\tadd-backup-server <node> <type> - Makes node a backup server keeping an up-to-date copy of all the shared state in the cluster; type may be ram for faster, non-persistant storage, or disk for data persistance\n\tstop <appname> <node> - Stops the given servlet on node\n\tmigrate <appname> <fromnode> <tonode> <port> - Migrates a servlet from fromnode to tonode, starting it on the given port\n\trunning - Prints a list of all the running applications on the cluster\n">>);
	"stats" -> % Display some stats
	    send(Sock, osp_manager:stats());
	"quit" ->
	    close(Sock),
	    exit(normal);
   	"shutdown" ->
	    send(Sock, "Shutting down\r\n"),
	    close(Sock),
	    osp_manager:shutdown_osp();
	"running" ->
	    NA = osp_manager:nodeapp(),
	    send(Sock, print_nodeapp(NA));
	"" ->
	    ok;
	Unknown -> % Handle multi-word command
	    Split = string:tokens(Unknown, " "),
	    [Comm | _] = Split, % Parse out the command from the arguments
	    case Comm of
		"start" ->
		    [Comm, AppList, PortList, NodeList ] = Split,
		    Port = erlang:list_to_integer(PortList),
		    App = erlang:list_to_atom(AppList),
		    Node = erlang:list_to_atom(NodeList),
		    case osp_manager:start_servlet(App, Port, Node) of
			ok ->
			    sendf(Sock, "~p started on ~p port ~p~n", [App, Node, Port]);
			{error, nonode} ->
			    send(Sock, "Sorry, the node you requested couldn't be found\r\n");
			{error, noapp} ->
			    send(Sock, "Sorry, the app you requested couldn't be found\r\n")
		    end;
		"migrate" ->
		    [Comm, AppList, FromNodeList, NodeList, PortList ] = Split,
		    Port = erlang:list_to_integer(PortList),
		    From = erlang:list_to_atom(FromNodeList),
		    App = erlang:list_to_atom(AppList),
		    Node = erlang:list_to_atom(NodeList),
		    case osp_manager:start_servlet(App, Port, Node) of
			ok ->
			    sendf(Sock, "~p started on ~p port ~p~n", [App, Node, Port]);
			{error, nonode} ->
			    send(Sock, "Sorry, the node you requested couldn't be found\r\n");
			{error, noapp} ->
			    send(Sock, "Sorry, the app you requested couldn't be found\r\n")
		    end,
		    case osp_manager:stop_servlet(App, From) of
			ok ->
			    sendf(Sock, "Stopped ~p on ~p ~n", [App, Node]);
			error ->
			    send(Sock, "Sorry, the node you requested couldn't be found\r\n")
		    end;
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
			    osp_manager:bkup_db(Node, ram_copies);
			disk ->
			    rpc:call(Node, mnesia, change_config, [extra_db_nodes, [node()]]),
			    rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies]),
			    osp_manager:bkup_db(Node, disc_copies);
			_ ->
			    send(Sock, "Sorry, unknown backup type: " ++ TypeL ++ "\r\n")
		    end;
		"stop" ->
		    [Comm, AppL, NodeL] = Split,
		    App = erlang:list_to_atom(AppL),
		    Node = erlang:list_to_atom(NodeL),
		    case osp_manager:stop_servlet(App, Node) of
			ok ->
			    sendf(Sock, "Stopped ~p on ~p ~n", [App, Node]);
			error ->
			    send(Sock, "Sorry, the node you requested couldn't be found\r\n")
		    end;
		_ ->
		    send(Sock, "Sorry, unknown command " ++ Unknown ++ "\r\n")
	    end
    end.

%% @doc Returns a human readable string of the nodeapp data
%% @spec print_nodeapp(list()) -> string()
print_nodeapp(NA) ->
    F1 = fun({App, Port}, Str) ->
		 Str ++ "\n    " ++ erlang:atom_to_list(App) ++ ":" ++ erlang:integer_to_list(Port)
	 end,
    F2 = fun({Node, Applist}, Str) ->
		 AppStr = lists:foldl(F1, "", Applist),
		 Str ++ erlang:atom_to_list(Node) ++ AppStr ++ "\n"
	 end,
    lists:foldl(F2, "", NA).

%% @doc Callback for the OSP broker service
%% @spec init() -> ok
init() ->
    ok.

%% @doc A callback for the OSP broker service
%% @spec cleanup() -> ok
cleanup() ->
    ok.
