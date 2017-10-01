%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%% Handles *ongoing* alarms where the intensity can be more or less (with
%%% periods of dark display), depending on the alarm type.
%%% Currently we support the following alarm types:
%%% + high_sensor_level : Intensive, happening once 30s then relaxed1_level
%%% + relaxed1_level    : Discrete, repeated every 60s
%%% + no_connection     : Discrete, repeated every 120s
%%% Note:
%%% - The alarm type is dynamic for an alarm and may change after some time of
%%%   ongoing alarm
%%%
%%%Some additional ideas for alarming includes:
%%% - If a new alarm occurs, while there already exists some other alarm,
%%%   short intense alarm for some time, then mix steady colour 
%%% - If data source is interrupted, use steady colour but do not mix.
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(moodle_alarm).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start/1,stop/1,
	 status/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("moodle_internal.hrl").

-define(SERVER, ?MODULE).

-record(state,{
	  alarm_db::list(),     % Known alarms
	  alarming::boolean(),  % Set to true if an alarm is running
	  alarm_type::atom(), % Type of alarm running, if any
	  cntr::integer()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


start(Type) ->
    gen_server:cast(?MODULE,{start_alarm,Type}).

stop(Type) ->
    gen_server:cast(?MODULE,{stop_alarm,Type}).

status() ->
    gen_server:call(?MODULE,status).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    AlarmDb=moodle_lib:get_cfg(moodle_alarms),
    {ok, #state{alarm_db=AlarmDb,
		alarming=false,
		cntr=0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(status, _From, State=#state{alarming=Alarming,
					alarm_type=AlarmType,
					cntr=Cntr}) ->
    Resp=if
	     Alarming ->
		 [{alarming,Alarming},{alarm_type,AlarmType},{cntr,Cntr}];
	     true ->
		 [{alarming,Alarming}]
	 end,
    {reply, Resp, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({start_alarm,AlarmType},State=#state{alarm_db=AlarmDb,
						    alarm_type=AlarmType0,
						    alarming=Alarming}) ->
    NewState=
	if
	    Alarming==false ->
		emd_log:debug("Raising new alarm ~p",[AlarmType]),
		case run_alarm(AlarmType,AlarmDb,0) of
		    {NewCntr,NewAlarmType} ->
			State#state{alarming=true,
				    cntr=NewCntr,alarm_type=NewAlarmType};
		    NewCntr ->
			State#state{alarming=true,
				    cntr=NewCntr,alarm_type=AlarmType}
		end;
	    true ->
		emd_log:debug("Raising alarm ~p, but was already alarming ~p",
			      [AlarmType,AlarmType0]),
		State#state{alarm_type=AlarmType}
    end,
    {noreply, NewState};
handle_cast({stop_alarm,_AlarmType}, State) ->
    {noreply, State#state{alarming=false,
			  alarm_type=undefined}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({timeout,_,alarming}, State=#state{alarm_db=AlarmDb,
					       alarming=Alarming,
					       alarm_type=AlarmType,
					       cntr=Cntr}) ->
    NewState=
	if
	    Alarming ->
		case run_alarm(AlarmType,AlarmDb,Cntr) of
		    {NewCntr,NewAlarmType} ->
			State#state{cntr=NewCntr,alarm_type=NewAlarmType};
		    NewCntr ->
			State#state{cntr=NewCntr}
		end;
	    true ->
		State
	end,
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Executes the actual alarm.
%% Note:
%% - Currently only possible to alarm using an extern Python program
run_alarm(AlarmType,AlarmDb,Cntr) ->
    case lists:keysearch(AlarmType,#alarm.type,AlarmDb) of
	{value,#alarm{max_cnt=MaxCntr,
		      source={external,ExtScript},
		      delay=Delay}} ->
	    Cmd=filename:join(code:priv_dir(moodle),ExtScript),
%	    os:cmd("sudo python "++Cmd);
	    io:format("os:cmd(\"sudo python \"++~p~n",[Cmd]),
	    erlang:start_timer(Delay,self(),alarming),
	    if
		MaxCntr==infinity ->
		    Cntr+1;
		Cntr==MaxCntr ->
		    case AlarmType of
			high_sensor_level -> {0,relaxed1_level};
			_ -> Cntr+1
		    end
	    end;
	Other ->
	    emd_log:error("Unknown alarm ~p, got ~p",[AlarmType,Other]),
	    Cntr
    end.
