%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(moodle_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    Childs = [#{id => moodle_manager,
		start => {moodle_manager, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [moodle_manager]},

	      #{id => moodle_alarm,
		start => {moodle_alarm, start_link, []},
		restart => permanent,
		shutdown => 5000,
		type => worker,
		modules => [moodle_alarm]}
	     ],

    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
