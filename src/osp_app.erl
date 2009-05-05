%% @copyright 2009 Jacob Torrey <torreyji@clarkson.edu>
%% @author Jacob Torrey <torreyji@clarkson.edu>
%% @doc Provides the application callbacks for OSP
-module(osp_app).
-behavior(application).

-export([start/2, stop/1]).

%% @doc Starts the OSP application
start(_Type, _StartArgs) ->
    supervisor_bridge:start_link({local, osp_supervisor}, osp_sup, []).

%% @doc Stops the OSP application
stop(_State) ->
    exit(whereis(osp_supervisor), shutdown).
