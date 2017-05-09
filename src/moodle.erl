%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>

-module(moodle).

%% API
-export([start/0,stop/0]).


%% @spec start() -> ok
%% @doc
%%  Start daisy and all applications it depends on, as given by configuration.
%% @end
start() ->
    application:start(?MODULE).


%% @spec stop() -> ok
%% @doc
%%  Stop all all running applications on this node except kernel and stdlib.
%% @end
stop() ->
    halt().
