-module(lib_hands).
-compile(export_all).

score_21([H|Cs]) ->
	score_21([H], Cs).
score_21(S, [H|Cs]) ->
	case lists:sum([H|S]) > 21 of
		true ->
			case lists:member(11, [H|S]) of
				true ->
					score_21([1|lists:delete(11, [H|S])], Cs);
				false ->
					score_21([H|S], Cs)
			end;
		false ->
			score_21([H|S], Cs)
	end;
score_21(S, []) ->
	lists:sum(S).
