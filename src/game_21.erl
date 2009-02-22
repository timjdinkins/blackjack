-module(game_21).
-behaviour(gen_fsm).

-export([init/1,
         handle_event/3,
         terminate/3,
         handle_info/3,
         handle_sync_event/4,
         code_change/4]).
-export([
  start_link/1,
  stop/0,
  betting/2,
  receiving_bets/2,
  dealing/2,
  playing_hands/2,
  playing_single_hand/2,
  dealer_hand/2,
  calc_hands/2,
  waiting_to_die/2
  ]).

start_link(Players) ->
	gen_fsm:start_link(?MODULE, [], Players).

stop() ->
	gen_fsm:send_all_state_event(?MODULE, terminate).

init(Ps) ->
	Players = dict:from_list([{P, {0, []}} || P <- Ps]),
	error_logger:error_msg("Initialized the game\n"),
	{ok, betting, {Players, deck:start()}, 0}.

betting(timeout, {Players, DeckPid}) ->
  error_logger:error_msg("betting:  notifying players\n"),
	lists:foreach(fun({P, _V}) -> player:place_bet(P, self()) end, dict:to_list(Players)),
	TimeoutPid = gen_fsm:send_event_after(30000, end_betting),
	{next_state, receiving_bets, {[], Players, DeckPid, TimeoutPid}}.

receiving_bets({bet, Pid, Amount}, {Received, Players, DeckPid, TimeoutPid}) ->
  error_logger:error_msg("receiving_bets, received a bet from ~w\n", [Pid]),
	R = lists:sort([Pid|Received]),
	case lists:sort(dict:fetch_keys(Players)) of
		R ->
			gen_fsm:cancel_timer(TimeoutPid),
			{next_state, dealing, {0, dict:store(Pid, {Amount, []}, Players), DeckPid}, 0};
		_ ->
			{next_state, receiving_bets, {R, Players, DeckPid}}
	end;
receiving_bets(end_betting, {_Received, Players, DeckPid, _TimeoutPid}) ->
  error_logger:error_msg("receiving_bets, ending betting\n"),
	{next_state, dealing, {Players, DeckPid}, 0}.

dealing(timeout, {Players, DeckPid}) ->
  error_logger:error_msg("dealing, dealing cards and notifying players of their hands.\n"),
	Players1 = dict:map(fun(_Pid, {Amt, _}) -> {Amt, deck:draw_cards(DeckPid, 2)} end, Players),
	lists:foreach(fun({Pid, {_, Cards}}) -> player:cards(Pid, Cards) end, dict:to_list(Players1)),
	Dealer = deck:draw_cards(DeckPid, 2),
	{next_state, playing_hands, {dict:fetch_keys(Players1), Players1, Dealer, DeckPid}, 0}.

playing_hands(timeout, {[], Players, Dealer, DeckPid}) ->
  error_logger:error_msg("playing_hands, done with all players, moving on to dealer hand\n"),
	{next_state, dealer_hand, {Players, Dealer, DeckPid}, 0};
playing_hands(timeout, {[K|Ks], Players, Dealer, DeckPid}) ->
  error_logger:error_msg("playing_hands, playing hand for ~w\n", [K]),
	player:hit_or_stay(K),
	TimeoutPid = gen_fsm:send_event_after(3000, timeout_player),
	{next_state, playing_single_hand, {K, Ks, Players, Dealer, TimeoutPid, DeckPid}}.

playing_single_hand(hit, {K, Ks, Players, Dealer, TimeoutPid, DeckPid}) ->
	Card = deck:draw_cards(DeckPid, 1),
	{ok, {Amt, Cards}} = dict:find(K, Players),
	case score_hand([Card|Cards]) of
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
	Score = play_dealer_hand(Dealer, DeckPid),
	{next_state, calc_hands, {Players, Score}, 0}.

calc_hands(timeout, {Players, DealerScore}) ->
	Notify = fun({Pid, {_, Cards}}) ->
						 case score_hand(Cards) of
							 Score when Score > DealerScore ->
								 player:win(Pid);
							 Score when Score == DealerScore ->
								 player:tie(Pid);
							 _ ->
								 player:lose(Pid)
						 end
					 end,
	lists:foreach(Notify, dict:to_list(Players)),
	{next_state, waiting_to_die, {Players, DealerScore}}.
	% Terminate the game now!

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
  error_logger:error_msg("calculating dealer hand with: ~w\n", [Cards]),
	case score_hand(Cards) of
		Score when Score > 21 ->
			0;
		Score when Score > 17 ->
			Score;
		_ ->
      NewCards = [deck:draw_cards(DeckPid, 1)|Cards],
      error_logger:error_msg("calculating dealer hand with: ~w\n", [NewCards]),
			play_dealer_hand(NewCards, DeckPid)
	end.

score_hand(Hand) ->
  Vals = [Val || {_Suit, _Card, Val} <- Hand],
	lib_hands:score_21(Vals).
