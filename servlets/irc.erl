-module(irc).
-behavior(osp_servlet).

% Export OSP server callback
-export([start_mnesia/0, server/1, init/0, cleanup/0, proto/0]).

% Import the OSP socket library
-import(osp_socket, [send/2, recv/2, sendf/3, close/1]).
% Import the OSP shared state library
-import(osp_mnesia, [start_atomic/0]).
% Import a configuration helper function
-import(osp, [get_conf/2]).

% Include the inet records for the hostent record
-include_lib("kernel/include/inet.hrl").

% Define the Mnesia record
-record(osp_table, {key, val}).

%% @doc Returns the IP protocol for the servlet
proto() ->    
    tcp.

%% @doc Opens a file for reading or writing
fopen(Filename, Flags) ->
    osp_file:fopen(irc, Filename, Flags).

%% @doc Stores a value in the mnesia database
store(Key, Val) ->
    osp_mnesia:store(irc_table, Key, Val).

%% @doc Gets a value from the mnesia database
retrieve(Key) ->
    osp_mnesia:retrieve(irc_table, Key).

%% @doc Flushes the transactional variables
flush() ->
    osp_mnesia:flush(irc_table).

%% @doc The mnesia startup routine
start_mnesia() ->
    case lists:member(mnesia_sup, erlang:registered()) of
	true ->
	        ok;
	false ->    mnesia:start()
    end,
    case catch(mnesia:table_info(irc_table, all)) of
	{'EXIT', _} ->
	        mnesia:create_table(irc_table, [{record_name, osp_table}, {attributes, record_info(fields, osp_table)}]);
	_ ->
	        ok
    end,
    ok.

chomp(Str) ->
    string:strip(string:strip(Str, both, $\n), both, $\r).

% User format: {nick, username, fullname, hostname, usermask, sendpid, channels}

server(Sock) ->
    register_user(Sock),
    rec_loop(Sock).

check_nick(Nick) ->
    case re:run(Nick, "( |\\\*|\\\||@)") of
	nomatch ->
	    true;
	_ ->
	    false
    end.

register_user(Sock) ->
    {ok, Hostname} = inet:gethostname(),
    Line = recv(Sock, 0),
    case Line of
	<<"NICK ", Rest/binary>> ->
	    Nick = chomp(erlang:binary_to_list(Rest)),
	    case check_nick(Nick) of
		true ->
		    case lists:keyfind(erlang:list_to_atom(Nick), 1, retrieve(users)) of
			false ->
			    Err = false;
			_ ->
			    send(Sock, ":" ++ Hostname ++ " 433 * " ++ Nick ++ " :Nickname name already in use\n"),
			    Err = true,
			    register_user(Sock)
		    end;
		false ->
		    Err = true,
		    send(Sock, ":" ++ Hostname ++ " 432 " ++ Nick ++ " :Invalid Nickname\n"),
		    register_user(Sock)
	    end;
	_ ->
	    Nick = "",
	    Err = true,
	    exit(normal)
    end,
    case Err of
	true ->
	    ok;
	_ ->
	    UL = erlang:binary_to_list(recv(Sock, 0)),
	    [CMD | Args] = string:tokens(UL, " "),
	    UCMD = string:to_lower(CMD),
	    case UCMD of
		"user" ->
		    [Login, Mask, _ | RealName] = Args,
		    put(nick, Nick),
		    {tcp, S} = Sock,
		    {ok, {Address, _}} = inet:peername(S),
		    {ok, Hostent} = inet:gethostbyaddr(Address),
		    case Hostent#hostent.h_name of
			HN when is_atom(HN) ->
			    Host = erlang:atom_to_list(HN);
			HN2 ->
			    Host = HN2
		    end,
		    store(users, retrieve(users) ++ [{erlang:list_to_atom(Nick)}]),
		    store(erlang:list_to_atom("user_" ++ Nick), {Nick, Login, string:strip(string:join(RealName, " "), left, $:), Host, Mask, spawn_link(fun() -> send_loop(Sock) end), []});
		_ ->
		    send(Sock, Hostname ++ " :Error\n")
	    end
    end.

rec_loop(Sock) ->
    Line = erlang:binary_to_list(recv(Sock, 0)),
    [Cmd | Args] = string:tokens(Line, " "),
    UCmd = string:to_lower(Cmd),
    case UCmd of
	"quit" ->
	    case Args of
		[] ->
		    Msg = " :Quit\n";
		_ ->
		    Msg = string:join(Args, " ")
	    end,
	    store(users, lists:keydelete(erlang:list_to_atom(get(nick)), 1, retrieve(users))),
	    {Nick, _Login, _Fullname, _Hostname, _UMask, Pid, Channels} = retrieve(erlang:list_to_atom("user_" ++ get(nick))),
	    F = fun(Channel) ->
			store(erlang:list_to_atom("channel_" ++ Channel), lists:keydelete(Pid, 1, retrieve(erlang:list_to_atom("channel_" ++ Channel)))),
			send(Nick, Channel, channel, "QUIT " ++ Msg)
		end,
	    lists:foreach(F, Channels),
	    store(erlang:list_to_atom("user_" ++ Nick), []),
	    Pid ! {quit},
	    exit(quit);
	"privmsg" ->
	    [Tar | MsgL] = Args,
	    Msg = string:join(MsgL, " "),
	    case Tar of
		[$# | _] ->
		    send(get(nick), Tar, channel, "PRIVMSG " ++ Msg);
		_ ->
		    send(get(nick), Tar, user, "PRIVMSG " ++ Msg)
	    end
    end,
    rec_loop(Sock).

send(From, To, Type, Msg) ->
    {Nick, Login, _Fullname, Hostname, _UMask, _Pid, _Channels} = retrieve(erlang:list_to_atom("user_" ++ From)),
    send(To, Type, ":" ++ Nick ++ "!" ++ Login ++ "@" ++ Hostname ++ " " ++ Msg).

send(To, channel, Msg) ->
    CInfo = retrieve(erlang:list_to_atom("channel_" ++ To)),
    case CInfo of 
	undefined ->
	    ok;
	{_Modes, Pids} ->
	    F = fun({Pid}) ->
			Pid ! {msg, Msg}
		end,
	    lists:foreach(F, Pids)
    end;
send(To, user, Msg) ->
    UInfo = retrieve(erlang:list_to_atom("user_" ++ To)),
    case UInfo of
	undefined ->
	    ok;
	{_Nick, _Login, _Fullname, _Hostname, _UMask, Pid, _Channels} ->
	    
	    Pid ! {msg, Msg}
    end.

%% @doc Provides a loop to send data to the client
send_loop(Sock) ->
    receive
	{msg, Msg} ->
	    case send(Sock, Msg) of
		{error, _} ->
		    ok;
		ok ->
		    send_loop(Sock)
	    end;
	{quit} ->
	    ok
    end.

init() ->
    case retrieve(users) of
	undefined ->
	    store(users, []);
	_ ->
	    ok
    end,
    case retrieve(channels) of
	undefined ->
	    store(channels, []);
	_ ->
	    ok
    end.

cleanup() ->
    store(users, []),
    store(channels, []),
    ok.
