%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc ERLFS Node supervisor module
-module(erlfs_sup).
-author('Jacob I. Torrey <torreyji@clarkson.edu>').

% External exports
-export([start/0, start/1, send/3, send_to_all/2]).

% Internal exports
-export([loop/0]).

%% @doc Starts the supervisor process and globally registers the name
start() ->
    start([]).

%% @doc Starts the supervisor process and globally registers the name (for the first machine)
%% @spec start(list()) -> atom()
start([]) ->
    Pid = spawn(?MODULE, loop, []),
    Name = erlang:list_to_atom(erlang:atom_to_list(node()) ++ "_sup"),
    global:register_name(Name, Pid),
    register(Name, Pid),
    erlfs_db:start(),
    erlfs_db:add_nodeinfo(node(), erlfs_fs:get_max_size(), erlfs_fs:get_filesystem_size()),
    Name;

%% @doc Starts the supervisor process and globally registers the name then connects to Node
%% @spec start(atom()) -> atom()
start(Node) ->
    Pid = spawn(?MODULE, loop, []),
    Name = erlang:list_to_atom(erlang:atom_to_list(node()) ++ "_sup"),
    global:register_name(Name, Pid),
    register(Name, Pid),
    send_to_all(monitor, {Name, node()}),
    erlfs_db:start(),
    erlfs_db:replicate(Node),
    erlfs_db:add_nodeinfo(node(), erlfs_fs:get_max_size(), erlfs_fs:get_filesystem_size()),
    Name.

%% @doc The main loop for the supervisor node
%% @spec loop() -> none()
loop() ->
    receive
	{From, sup_command, halt} ->
	    From ! {self(), sup_reply, halt},
	    erlfs_db:stop(),
	    exit(normal);
	{From, sup_command, delete, Fid} -> 
	    erlfs_fs:delete(Fid),
	    From ! {self(), sup_reply, delete},
	    loop();
	{From, sup_command, monitor, ID} ->
	    erlang:monitor(process, ID),
	    From ! {self(), sup_reply, monitor},
	    loop();
	{From, sup_command, replicate, Fid} ->
	    erlfs_fs:replicate(Fid),
	    From ! {self(), sup_reply, replicate},
	    loop();
	{From, sup_command, stats} ->
	    io:format("Status: running~nSpace: ~p/~p~nDir: ~p~n", [erlfs_fs:get_filesystem_size(), erlfs_fs:get_max_size(), erlfs_fs:get_dir()]),
	    From ! {self(), sup_reply, stats},
	    loop();
	{'DOWN', _, process, {_, Node}, _} ->
	    erlfs_db:try_delnodeinfo(Node),
	    erlfs_db:try_replicate(Node),
	    loop()
    end.

%% @doc Send a supervisor command message to a node supervisor
%% @spec send(atom(), any(), list()) -> ok | {error, Reason}
send(Node, Mesg, Args) ->
    Pid = global:whereis_name(erlang:list_to_atom(erlang:atom_to_list(Node) ++ "_sup")),
    case Args =:= [] of
	true ->
	    Pid ! {self(), sup_command, Mesg}; 
	false ->
	    Pid ! {self(), sup_command, Mesg, Args}
    end,
    receive
	{Pid, sup_reply, Mesg} ->
	    ok
    after 1000 ->
	    {error, 'no reply'}
    end.

%% @doc Send a supervisor command message to all nodes
%% @spec send_to_all(any(), list()) -> ok | {error, Reason}
send_to_all(Mesg, Args) ->
    Nodelist = nodes(),
    F = fun(Node) ->
		erlfs_sup:send(Node, Mesg, Args)
	end,
    lists:foreach(F, Nodelist).

