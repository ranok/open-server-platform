%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc The ERLFS administrative module. This module provides an administrator access to the filesystem
-module(erlfs_adm).
-author('Jacob I. Torrey <torreyji@clarkson.edu>').

-export([start/0, first_run/0]).

%% @doc The entry point for the administrative console
%% @spec start() -> none()
start() ->
    net_kernel:start([erlfs_conf:get_conf(nodename)]),
    io:format("Welcome to the ERLFS administrative console~n~n", []),
    ioloop().

%% @doc The main admin loop
%% @spec ioloop() -> none()
ioloop() ->
    Line = io:get_line('> '),
    case string:sub_string(Line, 1, string:len(Line) - 1) of
	"quit" ->
	    halt();
	"help" ->
	    io:format("ERLFS Admin Console~n~nquit - Quits this console~nhelp - Prints this help~nstart - Starts ERLFS on this node~nstop - Stops ERLFS~nstats - Displays some stats about the ERLFS node~n", []),
	    ioloop();
	"start" ->
	    Name = erlfs_sup:start(),
	    io:format("Started supervisor: ~p~n", [Name]),
	    ioloop();
	"stop" ->
	    erlfs_sup:send(node(), halt, []),
	    rpc:call(node(), init, stop, []),
	    io:format("Stopping ERLFS~n"),
	    ioloop();
	"stats" ->
	    io:format("ERLFS Node Info:~n", []),
	    erlfs_sup:send(node(), stats, []),
	    ioloop();
	_ ->
	    io:format("Invalid command~n", []),
	    ioloop()    
    end.

%% @doc Run the first time to set everything up
%% @spec first_run() -> none()
first_run() ->
    net_kernel:start([erlfs_conf:get_conf(nodename)]),
    io:format("Setting up the metadata store~n", []),
    erlfs_db:format(),
    erlfs_db:stop(),
    io:format("Initial configuration complete!~n", []),
    halt().

