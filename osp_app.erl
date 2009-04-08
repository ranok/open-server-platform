%% @copyright 2008 Jacob Torrey
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc OSP Application Calback module
-module(osp_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Args) ->
    osp:start().

stop(_State) ->
    osp:stop().
