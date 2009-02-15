-module(deck).
-export([start/0, draw_cards/2]).

-define (SUITS, [hearts, diamonds, clubs, spades]).
-define (CARDS, [{2,2},{3,3},{4,4},{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}, {jack,10}, {queen,10}, {king, 10}, {ace, 11}]).

start() ->
  Deck = shuffle(deck()),
  spawn(fun() -> loop(Deck) end).

deck() ->
  F = fun(S) -> [{S, C, V} || {C, V} <- ?CARDS] end,
	lists:flatten(lists:map(F, ?SUITS)).

draw_cards(Pid, N) when N > 0 ->
  Pid ! {self(), draw, N},
  receive
    {ok, Cards} -> Cards;
    {error, Res} -> {error, Res}
  end.

loop(Deck) ->
  receive
    {Pid, draw, N} ->
      case draw(Deck, N) of
        {error, Res} ->
          Pid ! {error, Res},
          loop(Deck);
        {Deck1, Cards} ->
          Pid ! {ok, Cards},
          loop(Deck1)
      end
  end.

draw(D, N) when N > length(D) ->
	{error, too_many_cards_requested};
draw([H|T], N) when N > 0 ->
  draw(T, [H], N - 1);
draw([], _) ->
  {error, empty_deck}.
draw(Deck, Cards, 0) ->
  {Deck, Cards};
draw([H|T], Cards, N) ->
  draw(T, [H|Cards], N - 1).

shuffle(List) ->
	%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) -> {random:uniform(), A} end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.
