-module(player).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3]).
-export([start_link/0, stop/1]).
-export([join_table/1, bet/2, cards/2]).


start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

join_table(Pid) ->
	gen_server:cast(Pid, join),
  ok.

bet(Pid, Amount) ->
	gen_server:cast(Pid, {bet, Amount}).

cards(Pid, Cards) ->
	gen_server:cast(Pid, {cards, Cards}).

init([]) ->
	{ok, undef}.

handle_call(stop, _From, GamePid) ->
  {stop, normal, GamePid}.

handle_cast(join, GamePid) ->
	table:join(self()),
	{noreply, GamePid};

handle_cast({game_pid, Pid}, _GamePid) ->
  {noreply, Pid};

handle_cast({bet, Amount}, GamePid) ->
	game_21:bet(GamePid, {bet, self(), Amount}),
	{noreply, GamePid};

handle_cast({cards, Cards}, GamePid) ->
	oi:format("Your cards: ~p.~n", [Cards]),
	{noreply, GamePid};

handle_cast(stop, State) ->
	{stop, normal, State}.

code_change(_Vsn, State, _Extra) ->
  {ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(normal, _State) ->
	ok.
