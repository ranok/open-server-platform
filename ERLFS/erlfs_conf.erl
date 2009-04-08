%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc A module that uses dets to store and retrieve the configuration information
-module(erlfs_conf).
-author('Jacob Torrey <torreyji@clarkson.edu>').

-export([get_conf/1]).

% Defines the configuration file
-define(CONFIGFILE, "erlfs.conf").

%% @doc Returns the value of the configuration file for the passed key
%% @spec get_conf(atom()) -> any()
get_conf(Key) ->
    {ok, Conf} = file:consult(?CONFIGFILE),
    Res = lists:keysearch(Key, 1, Conf),
    case Res of
	false ->
	    [];
	{value, {Key, Val}} ->
	    Val
    end.
