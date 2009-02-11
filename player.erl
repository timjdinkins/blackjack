-module(player).

-behaviour(gen_server).

-export([start/0, stop/1]).
-export([init/1, handle_cast/2]).
-export([join_table/1]).

start() ->
  gen_server:start(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([]) ->
	{ok, undef}.

join_table(Pid) ->
	gen_server:cast(Pid, join).

bet(Pid, Amount) ->
	gen_server:cast(Pid, {bet, Amount}).

cards(Pid, Cards) ->
	gen_server:cast(Pid, {cards, Cards}).

handle_cast(join, GamePid) ->
	table:join(self()),
	{noreply, GamePid};

handle_cast({game_pid, Pid}, _GamePid) ->
	{noreply, GamePid};

handle_cast({bet, Amount}, GamePid) ->
	game_21:bet(GamePid, {bet, self(), Amount});

handle_cast({cards, Cards}, GamePid) ->
	oi:format("Your cards: ~p.~n", [Cards]).

handle_cast(stop, State) ->
	{stop, normal, State}.

terminate(normal, _State) ->
	ok.