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

-define(NETWORK, "OSP").

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

server(Sock) ->
    put(lines, []),
    rec_loop(Sock).

check_nick(Nick) ->
    case re:run(Nick, "( |\\\*|\\\||@)") of
	nomatch ->
	    true;
	_ ->
	    false
    end.

get_line(Sock) ->
    case get(lines) of
	[] ->
	    Line = erlang:binary_to_list(recv(Sock, 0)),
	    [Ret | Rest] = string:tokens(Line, "\r\n"),
	    put(lines, Rest),
	    Ret;
	[Ret | Rest] ->
	    put(lines, Rest),
	    Ret
    end.

% User format: {nick, username, fullname, hostname, usermask, sendpid, channels}

rec_loop(Sock) ->
    {ok, Hostname} = inet:gethostname(),
    Line = get_line(Sock),
    [Cmd | Args] = string:tokens(Line, " "),
    UCmd = string:to_lower(Cmd),
    case UCmd of
	"nick" ->
	    [Nick1] = Args,
	    Nick = chomp(Nick1),
	    case check_nick(Nick) of
		true ->
		    case lists:keyfind(erlang:list_to_atom(Nick), 1, retrieve(users)) of
			false ->
			    case get(uinfo) of
				{_ONick, undefined, undefined, undefined, undefined, undefined, []} ->
				    put(uinfo, {Nick, undefined, undefined, undefined, undefined, undefined, []});
				undefined ->
				    put(uinfo, {Nick, undefined, undefined, undefined, undefined, undefined, []});
				{undefined, Username, Fullname, Host, Usermask, undefined, []} ->
				    put(nick, Nick),
				    new_user({Nick, Username, Fullname, Host, Usermask, undefined, []}, Sock)
			    end;
			_ ->
			    send(Sock, ":" ++ Hostname ++ " 433 * " ++ Nick ++ " :Nickname name already in use\r\n")
		    end;
		false ->
		    send(Sock, ":" ++ Hostname ++ " 432 " ++ Nick ++ " :Invalid Nickname\r\n")
	    end;
	"user" ->
	    [Login, Mask, _ | RealName] = Args,
	    {tcp, S} = Sock,
	    {ok, {Address, _}} = inet:peername(S),
	    {ok, Hostent} = inet:gethostbyaddr(Address),
	    case Hostent#hostent.h_name of
		HN when is_atom(HN) ->
		    RHost = erlang:atom_to_list(HN);
		HN2 ->
		    RHost = HN2
	    end,
	    Fullname = string:strip(string:join(RealName, " "), left, $:),
	    case get(uinfo) of
		{undefined, _OUsername, _OFullname, _OHost, _OUsermask, undefined, []} ->
		    put(uinfo, {undefined, Login, Fullname, RHost, Mask, undefined, []});
		undefined ->
		    put(uinfo, {undefined, Login, Fullname, RHost, Mask, undefined, []});
		{Nick, undefined, undefined, undefined, undefined, undefined, []} ->
		    put(nick, Nick),
		    new_user({Nick, Login, Fullname, RHost, Mask, undefined, []}, Sock)
	    end;
	"join" ->
	    {N, L, F, H, M, Pid, UChans} = retrieve(erlang:list_to_atom("user_" ++ get(nick))),
	    case Args of
		[Chans, Keys] ->
		    Chanlist = string:tokens(Chans, ","),
		    Keylist = string:tokens(Keys, ",");
		[Chans] ->
		    Chanlist = string:tokens(Chans, ","),
		    Keylist = []
	    end,
	    Func = fun(Chan) -> %% @todo Check for channel key
			send(N, Chan, channel, "JOIN :" ++ Chan ++ "\r\n"),
			case retrieve(erlang:list_to_atom("channel_" ++ Chan)) of
			    undefined ->
				store(channels, retrieve(channels) ++ [Chan]),
				store(erlang:list_to_atom("channel_" ++ Chan), {[{"n"}, {"t"}], [{Pid}]});
			    {Modes, Pids} ->
				store(erlang:list_to_atom("channel_" ++ Chan), {Modes, Pids ++ [{Pid}]})
			end
		end,
	    lists:foreach(Func, Chanlist),
	    store(erlang:list_to_atom("user_" ++ get(nick)), {N, L, F, H, M, Pid, UChans ++ Chanlist});
	"quit" ->
	    case Args of
		[] ->
		    Msg = " :Quit\r\n";
		_ ->
		    Msg = string:join(Args, " ")
	    end,
	    case get(nick) of
		undefined ->
		    ok;
		_ ->
		    store(users, lists:keydelete(erlang:list_to_atom(get(nick)), 1, retrieve(users))),
		    {Nick, _Login, _Fullname, _Hostname, _UMask, Pid, Channels} = retrieve(erlang:list_to_atom("user_" ++ get(nick))),
		    F = fun(Channel) ->
				{Modes, Pids} = retrieve(erlang:list_to_atom("channel_" ++ Channel)),
				store(erlang:list_to_atom("channel_" ++ Channel), {Modes, lists:keydelete(Pid, 1, Pids)}),
				send(Nick, Channel, channel, "QUIT " ++ Msg)
			end,
		    lists:foreach(F, Channels),
		    store(erlang:list_to_atom("user_" ++ Nick), []),
		    Pid ! {quit}
	    end,
	    put(uinfo, undefined),
	    exit(quit);
	"privmsg" ->
	    Nick = get(nick),
	    if
		Nick =:= undefined ->
		    send(Sock, ":" ++ Hostname ++ " 451 PRIVMSG :Not registered\r\n");
		true ->
		    [Tar | MsgL] = Args,
		    Msg = string:join(MsgL, " "),
		    case Tar of
			[$# | _] ->
			    send(get(nick), Tar, channel, "PRIVMSG " ++ Msg ++ "\r\n");
			_ ->
			    send(get(nick), Tar, user, "PRIVMSG " ++ Msg ++ "\r\n")
		    end
	    end;
	Other ->
	    Nick = get(nick),
	    if 
		Nick =:= undefined ->
		    send(Sock, ":" ++ Hostname ++ " 451 PRIVMSG :Not registered\r\n");
		true ->
		    send(Sock, ":" ++ Hostname ++ " 421 " ++ string:to_upper(Other) ++ " :Unknown command\r\n")
	    end
    end,
    rec_loop(Sock).

new_user({Nick, Username, Fullname, Hostname, Usermask, undefined, []}, Sock) ->
    {ok, SHostname} = inet:gethostname(),
    store(users, retrieve(users) ++ [{erlang:list_to_atom(Nick)}]),
    Pid = spawn_link(fun() -> send_loop(Sock) end),
    send(Sock, ":" ++ SHostname ++ " 001 " ++ Nick ++ " :Welcome to the " ++ ?NETWORK ++ " " ++ Nick ++ "!" ++ Username ++ "@" ++ Hostname ++ "\r\n"),
    send(Sock, ":" ++ SHostname ++ " 002 " ++ Nick ++ " :Your host is " ++ SHostname ++ ", running ospIRCD version 0.1!\r\n"),
    send(Sock, ":" ++ SHostname ++ " 003 " ++ Nick ++ " :This server created Dec. 27, 2009\r\n"),
    store(erlang:list_to_atom("user_" ++ Nick), {Nick, Username, Fullname, Hostname, Usermask, Pid, []}).


send(From, To, channel, Msg) when is_pid(From) ->
    CInfo = retrieve(erlang:list_to_atom("channel_" ++ To)),
    case CInfo of 
	undefined ->
	    ok;
	{_Modes, Pids} ->
	    F = fun({Pid}) ->
			if
			    Pid == From ->
				ok;
			    true ->
				Pid ! {msg, Msg}
			end
		end,
	    lists:foreach(F, Pids)
    end;
send(From, To, user, Msg) when is_pid(From) ->
    UInfo = retrieve(erlang:list_to_atom("user_" ++ To)),
    case UInfo of
	undefined ->
	    ok;
	{_Nick, _Login, _Fullname, _Hostname, _UMask, Pid, _Channels} ->   
	    Pid ! {msg, Msg}
    end;
send(From, To, Type, Msg) ->
    {Nick, Login, _Fullname, Hostname, _UMask, Pid, _Channels} = retrieve(erlang:list_to_atom("user_" ++ From)),
    send(Pid, To, Type, ":" ++ Nick ++ "!" ++ Login ++ "@" ++ Hostname ++ " " ++ Msg).

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
