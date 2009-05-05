%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc Provides a way for OSP to be integrated into an OTP tree
-module(osp_sup).
-behavior(supervisor_bridge).

-export([init/1, terminate/2]).

%% @doc Provides an OTP-eqse init function for application integration
%% @spec init(list()) -> {ok, pid(), pid()} | {error, Reason} 
init(_Args) ->
    osp:start().

%% @doc Shuts down the OSP application
%% @spec terminate(atom(), pid()) -> ok
terminate(_Reason, State) ->
    osp:stop(State).
