%%%-------------------------------------------------------------------
%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2017 by Johan <>
%%%-------------------------------------------------------------------
-module(moodle_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_alarm/0,stop_alarm/0,
	 load_cfg/0,
	 status/0,status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("moodle_internal.hrl").

-define(SERVER, ?MODULE).

-record(state,{
	  data_source::tuple(), % RPC arguments, to get the sensor data from
	  fetch_interval::integer(), % Time, in seconds, between a fetch of data
	  alarm_triggers::list(),    % Sensor level(s) to trigger alarm(s)
	  alarm_counters::list()
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


start_alarm() ->
    gen_server:cast(?MODULE,start_alarm).

stop_alarm() ->
    gen_server:cast(?MODULE,stop_alarm).

status() ->
    gen_server:call(?MODULE,{status,all}).

status(Key) ->
    gen_server:call(?MODULE,{status,Key}).

load_cfg() ->
    gen_server:call(?MODULE,load_cfg).


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
    DsNMFA=moodle_lib:get_cfg(moodle_external,undefined),
    FetchInterval=moodle_lib:get_cfg(moodle_fetch_interval),
    TriggerLevels=moodle_lib:get_cfg(moodle_alarm_triggers),
    start_timer(FetchInterval),
    {ok, #state{data_source=DsNMFA,
		fetch_interval=FetchInterval,
		alarm_triggers=TriggerLevels,
		alarm_counters=[]}}.

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
handle_call({status,Key}, _From, State) ->
    %% Read the current status    
    Reply=
	if Key==all -> [{alarm,moodle_alarm:status()},{temp,read_temp()}];
	   Key==alarm -> moodle_alarm:status();
	   Key==temp -> read_temp();
	   true -> {error,unknown_status}
	end,
    {reply, Reply, State};
handle_call(load_cfg, _From, State) ->
    %% Read the current status    
    DsNMFA=moodle_lib:get_cfg(moodle_external,undefined),
    FetchInterval=moodle_lib:get_cfg(moodle_fetch_interval),
    TriggerLevels=moodle_lib:get_cfg(moodle_alarm_triggers),
    NewState=State#state{data_source=DsNMFA,
			 fetch_interval=FetchInterval,
			 alarm_triggers=TriggerLevels},
    {reply, ok, NewState}.


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
handle_cast(fetch_data, State=#state{fetch_interval=FetchInterval,
				     alarm_triggers=TriggerLevels,
				     data_source={N,M,F,A},
				     alarm_counters=AlarmCnt}) ->
    %% Fetch sensor data from the server
    %% Prior to get this fully working, we should also need to register this
    %% application in the server.
    %% This includes:
    %% - Give this aplication a role
    %% - Give this aplication just enough capabillities to fulfill its task
    NewAlarmCnt=
	try case rpc:call(N,M,F,A) of
		V when is_integer(V);is_float(V) ->
		    emd_log:debug("V=~p TriggerLevel=~p",[V,TriggerLevels]),
		    trigger_alarms({co2,V},TriggerLevels,AlarmCnt);
	        _ ->
		    emd_log:debug("No/Unknown data",[]),
		    trigger_alarms(failing,TriggerLevels,AlarmCnt)
	    end
	catch
	    _:Reason ->
		emd_log:error("Could not reach ~p:~p at ~p, got ~p",
			      [M,F,N,Reason]),
		trigger_alarms(failing,TriggerLevels,AlarmCnt)
	end,
    start_timer(FetchInterval),
    {noreply, State#state{alarm_counters=NewAlarmCnt}};
handle_cast(start_alarm, State) ->
    %% Manually force start of alarm
    moodle_alarm:start(high_sensor_level),
    {noreply, State};
handle_cast(stop_alarm, State) ->
    %% Manually force stop of alarm
    moodle_alarm:stop(high_sensor_level),
    {noreply, State}.

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
handle_info(_Info, State) ->
    {noreply, State}.

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

trigger_alarms(failing,TriggerLevels,AlarmCnt) ->
    case increase_cntrs(failing,TriggerLevels,AlarmCnt) of
	{new_alarm,AlarmType2,NAC} ->
	    moodle_alarm:start(AlarmType2),
	    NAC;
	NAC ->
	    NAC
    end;
trigger_alarms({Src,Data},TriggerLevels,AlarmCnt) ->
    #trigger{value=TriggerLevel,
	     alarm_type=AlarmType}=lookup_trigger(TriggerLevels,Src,limit),
    if
	Data>TriggerLevel ->
	    %% Increase counter
	    case increase_cntrs(Src,TriggerLevels,AlarmCnt) of
		{new_alarm,AlarmType2,NAC} ->
		    moodle_alarm:start(AlarmType2),
		    NAC;
		NAC ->
		    moodle_alarm:start(AlarmType),
		    NAC
	    end;
	true ->
	    %% Stop alarms and reset counters
	    reset_alarms(Src,AlarmType,TriggerLevels,AlarmCnt)
    end.


%% Increase the value of the "Key" counter with one.
increase_cntrs(Src,TriggerLevels,AlarmCnt0) ->
    {CntVal,AlarmCnt1}=increase_cntr(AlarmCnt0,{Src,cnt},undefined,[]),
    #trigger{value=TriggerLevel,
	     alarm_type=AlarmType2}=lookup_trigger(TriggerLevels,Src,cnt),
    if
	CntVal>TriggerLevel ->
	    {new_alarm,AlarmType2,AlarmCnt1};
	true ->
	    AlarmCnt1
    end.



%% Increase the value of the "Key" counter with one.
increase_cntr([],Key,undefined,Out) ->
    {1,lists:reverse([{Key,1}|Out])};
increase_cntr([],_Key,CntVal,Out) ->
    {CntVal,lists:reverse(Out)};
increase_cntr([{Key,Val}|Rest],Key,_CntVal,Out) ->
    increase_cntr(Rest,Key,Val+1,[{Key,Val+1}|Out]);
increase_cntr([H|Rest],Key,CntVal,Out) ->
    increase_cntr(Rest,Key,CntVal,[H|Out]).


reset_alarms(Src,AlarmType1,TriggerLevels,AlarmCnt) ->
    moodle_alarm:stop(AlarmType1),
    #trigger{alarm_type=AlarmType2}=lookup_trigger(TriggerLevels,Src,cnt),
    moodle_alarm:stop(AlarmType2),
    lists:keydelete({Src,cnt},1,AlarmCnt).
    


lookup_trigger([],_Src,_Type) ->    
    not_found;
lookup_trigger([H=#trigger{src=Src,type=Type}|_],Src,Type) -> 
    H;
lookup_trigger([_|Rest],Src,Type) -> 
    lookup_trigger(Rest,Src,Type).




start_timer(FetchInterval) ->
    erlang:send_after(FetchInterval*1000,self(),{'$gen_cast',fetch_data},[]).




%% Assumes we are running this on a Raspberry Pi with Rasbian installed ...
read_temp() ->
    case os:cmd("/opt/vc/bin/vcgencmd measure_temp") of
	"temp="++TempStr ->
	    io:format("TempStr=~p~n",[TempStr]),
	    temp_to_float(TempStr,[]);
	Other ->
	    emd_log:warning("Unexpected temperature ~p",[Other]),
	    undefined
    end.

temp_to_float([],Out) ->
    Str=lists:reverse(Out),
    list_to_float(Str);
temp_to_float([H|Rest],Out) when $0=<H,H=<$9 ->
    temp_to_float(Rest,[H|Out]);
temp_to_float([$.|Rest],Out) ->
    temp_to_float(Rest,[$.|Out]);
temp_to_float(_,Out) ->
    Str=lists:reverse(Out),
    list_to_float(Str).

    
