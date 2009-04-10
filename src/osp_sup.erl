%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc Provides a way for OSP to be integrated into an OTP tree
-module(osp_sup).
-behavior(supervisor_bridge).

-export([init/1, terminate/2]).

init(_Args) ->
    osp:start().

terminate(_Reason, _State) ->
    osp:stop().
