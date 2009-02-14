-module(table).
-behaviour(gen_fsm).

-export([init/1,
         handle_event/3,
         terminate/3,
         handle_info/3,
         handle_sync_event/4,
         code_change/4]).
-export([
  start/0,
  stop/0,
  join/1,
  quit/1,
  players/0,
  waiting/3,
  playing/3,
  playing/2]).

start() ->
	gen_fsm:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_fsm:send_all_state_event(?MODULE, stop).

init([]) ->
	{ok, waiting, {undef, []}}.

join(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {join, Pid}).

quit(Pid) ->
	gen_fsm:sync_send_event(?MODULE, {quit, Pid}).

players() ->
	gen_fsm:sync_send_all_state_event(?MODULE, players).

waiting({join, Pid}, _From, {GamePid, Players}) ->
	io:format("Added a player.  Going from waiting to playing.~n"),
	{reply, ok, playing, {GamePid, [Pid|Players]}, 0}.

playing({join, Pid}, _From, {GamePid, Players}) ->
	case lists:member(Pid, Players) of
		true ->
			{reply, ok, playing, {GamePid, Players}};
	 	false ->
			{reply, ok, playing, {GamePid, [Pid|Players]}}
		end;
playing({quit, Pid}, _From, {GamePid, Players}) ->
	case lists:delete(Pid, Players) of
		Players ->
			{reply, {error, no_player_matched}, playing, {GamePid, Players}};
		NewPlayers ->
			case length(NewPlayers) of
				0 ->
					{reply, ok, waiting, {GamePid, NewPlayers}};
				_ ->
				{reply, ok, playing, {GamePid, NewPlayers}}
			end
	end.

playing(terminate_game, {GamePid, Players}) ->
	game_21:stop(GamePid),
	{ok, Pid} = game_21:start(Players),
	{next_state, playing, {Pid, Players}};
playing(timeout, {_GamePid, Players}) ->
	{ok, Pid} = game_21:start(Players),
	{next_state, playing, {Pid, Players}}.

handle_sync_event(players, _From, StateName, {GamePid, Players}) ->
	{reply, Players, StateName, {GamePid, Players}}.

handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData}.

handle_info(_Info, StateName, State) ->
  {ok, StateName, State}.

terminate(normal, _StateName, _StateData) ->
	ok;
terminate(Reason, _StateName, _StateData) ->
	io:format("Abnormal shutdown.  Reason: ~p~n", [Reason]),
	ok.

code_change(_Vsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
