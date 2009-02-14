-module(game_21).
-behaviour(gen_fsm).

-export([init/1,
         handle_event/3,
         terminate/3,
         handle_info/3,
         handle_sync_event/4,
         code_change/4]).
-export([
  start/1,
  stop/1,
  betting/2,
  receiving_bets/2,
  dealing/2,
  playing_hands/2,
  playing_single_hand/2,
  dealer_hand/2,
  calc_hands/2,
  waiting_to_die/2
  ]).

start(Players) ->
	gen_fsm:start(?MODULE, [], {Players, deck:start()}).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init({Ps, DeckPid}) ->
	Players = dict:from_list([{P, {0, []}} || P <- Ps]),
	{ok, betting, {Players, DeckPid}, 0}.

betting(timeout, {Players, DeckPid}) ->
	lists:foreach(fun({P, _V}) -> player:place_bet(P, self()) end, dict:to_list(Players)),
	TimeoutPid = gen_fsm:send_event_after(30000, end_betting),
	{next_state, receiving_bets, {[], Players, DeckPid, TimeoutPid}}.

receiving_bets({bet, Pid, Amount}, {Received, Players, DeckPid, TimeoutPid}) ->
	R = lists:sort([Pid|Received]),
	case lists:sort(dict:fetch_keys(Players)) of
		R ->
			gen_fsm:cancel_timer(TimeoutPid),
			{next_state, dealing, {0, dict:store(Pid, {Amount, []}, Players), DeckPid}, 0};
		_ ->
			{next_state, receiving_bets, {R, Players, DeckPid}}
	end;
receiving_bets(end_betting, {_Received, Players, DeckPid, _TimeoutPid}) ->
	{next_state, dealing, {Players, DeckPid}, 0}.

dealing(timeout, {Players, DeckPid}) ->
	Players1 = dict:map(fun(_Pid, {Amt, _}) -> {Amt, deck:draw_cards(DeckPid, 2)} end, Players),
	dict:each(fun(Pid, {_, Cards}) -> player:cards(Pid, Cards) end, Players1),
	Dealer = deck:draw_cards(DeckPid, 2),
	{next_state, playing_hands, {dict:fetch_keys(Players1), Players1, Dealer, DeckPid}, 0}.

playing_hands(timeout, {[], Players, Dealer, DeckPid}) ->
	{next_state, dealer_hand, {Players, Dealer, DeckPid}, 0};
playing_hands(timeout, {[K|Ks], Players, Dealer, DeckPid}) ->
	player:hit_or_stay(K),
	TimeoutPid = gen_fsm:send_event_after(30000, timeout_player),
	{next_state, playing_single_hand, {K, Ks, Players, Dealer, TimeoutPid, DeckPid}}.

playing_single_hand(hit, {K, Ks, Players, Dealer, TimeoutPid, DeckPid}) ->
	Card = deck:draw_cards(DeckPid, 1),
	{ok, {Amt, Cards}} = dict:find(K, Players),
	case lib_hands:score_21([Card|Cards]) of
		Score when Score > 21 ->
			gen_fsm:cancel_timer(TimeoutPid),
			{next_state, playing_hands, {Ks, Players, Dealer, DeckPid}, 0};
		_ ->
			Players1 = dict:store(K, {Amt, [Card|Cards]}),
			{next_state, playing_single_hand, {K, Ks, Players1, Dealer, TimeoutPid, DeckPid}}
	end;
playing_single_hand(stay, {_K, Ks, Players, Dealer, TimeoutPid, DeckPid}) ->
	gen_fsm:cancel_timer(TimeoutPid),
	{next_state, playing_hands, {Ks, Players, Dealer, DeckPid}, 0};
playing_single_hand(timeout_player, {_, Ks, Players, Dealer, DeckPid}) ->
	{next_state, playing_hands, {Ks, Players, Dealer, DeckPid}, 0}.

dealer_hand(timeout, {Players, Dealer, DeckPid}) ->
	Dealer1 = play_dealer_hand(Dealer, DeckPid),
	{next_state, calc_hands, {Players, Dealer1}, 0}.

calc_hands(timeout, {Players, Dealer}) ->
	DScore = lib_hands:score_21(Dealer),
	Notify = fun({Pid, {_, Cards}}) ->
						 case lib_hands:score_21(Cards) of
							 Score when Score > DScore ->
								 player:win(Pid);
							 Score when Score =:= DScore ->
								 player:tie(Pid);
							 _ ->
								 player:lose(Pid)
						 end
					 end,
	lists:foreach(Notify, dict:to_list(Players)).
	%% Terminate the game now!

waiting_to_die(Any, State) ->
	io:format("Received: ~p.  I shouldn't receive anything except a terminate message.~n", [Any]),
	{next_state, waiting_to_die, State}.

handle_event(stop, _StateName, StateData) ->
	{stop, normal, StateData}.

handle_sync_event(Event, _From, StateName, State) ->
  io:format("Unknown event: ~w~n", [Event]),
  {ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {ok, StateName, State}.

terminate(normal, _StateName, _State) ->
	ok;
terminate(Reason, _StateName, _State) ->
	io:format("Abnormal shutdown.  Reason: ~p~n", [Reason]),
	ok.

code_change(_Vsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

play_dealer_hand(Cards, DeckPid) ->
	case lib_hands:score_21(Cards) of
		Score when Score > 21 ->
			0;
		Score when Score > 17 ->
			Score;
		_ ->
			play_dealer_hand([deck:draw_cards(DeckPid, 1)|Cards], DeckPid)
	end.
