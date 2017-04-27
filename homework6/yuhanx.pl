:-[airlines].

member(X, [X|_]) :- !.
member(X, [_|L]) :-
	member(X, L).

print( [ ] ).
print( [ X | Y ] ):- nl, write(X), print( Y ).

not( X ) :- X, !, fail.
not( _ ).

list(X, [X]).

is_list(X) :-
        var(X), !,
        fail.
is_list([]).
is_list([_|T]) :-
        is_list(T).

last1([_ | Rest], Last) :- last1(Rest, Last).
last1([Last], Last).

flight_leg([City1, Airline, City2]):-
  flight(Airline, City1, City2, _, _);
  flight(Airline, City2, City1, _, _).

flight_path([]).
flight_path([X|Xs]):-
  flight_leg(X),
  flight_path(Xs).

check_valid(_, []).
check_valid(X, [Y|Ys]):-
  flight_leg(X),
  X = [_, _,City1],
  Y = [City2, _, _],
  City1 = City2,
  check_valid(Y, Ys).

valid_flight_path([X|Xs]):-
  check_valid(X, Xs).

price_of_a_path([], Price, Price).
price_of_a_path([X|Xs], CalPrice, Price):-
  flight_leg(X),
  X = [City1, Airline, City2],
  airport(City1, Tax, _),
  (
    flight(Airline, City1, City2, Cost, _);
    flight(Airline, City2, City1, Cost, _)
  ),
  NewPrice is CalPrice + Tax + Cost,
  price_of_a_path(Xs, NewPrice, Price).

duration_of_a_path([], Duration, Duration).
duration_of_a_path([X|Xs], CalDuration, Duration):-
  flight_leg(X),
  X = [City1, Airline, City2],
  airport(City1, _, Delay),
  (
    flight(Airline, City1, City2, _, Time);
    flight(Airline, City2, City1, _, Time)
  ),
  NewDuration is CalDuration + Delay + Time,
  duration_of_a_path(Xs, NewDuration, Duration).

num_of_airlines([], Num, Num, _).
num_of_airlines([X|Xs], CalNum, Num, Airlines):-
  flight_leg(X),
  X = [_, Airline, _],
  (not(member(Airline, Airlines))->
    NewNum is CalNum + 1,
    append([Airline], Airlines, NewAirlines);
   member(Airline, Airlines)->
     NewNum is CalNum
  ),
  num_of_airlines(Xs, NewNum, Num, NewAirlines).

flight_route(Price, Duration, NumAirlines, [X|Xs]):-
  valid_loop_free_path([X|Xs], []),
  price_of_a_path([X|Xs], 0, Price),
  duration_of_a_path([X|Xs], 0, Duration),
  num_of_airlines([X|Xs], 0, NumAirlines, []).

valid_loop_free_path([X], Airports):-
  flight_leg(X),
  X = [City1, _, City2],
  not(member(City1, Airports)),
  not(member(City2, Airports)).

valid_loop_free_path([X|Xs], Airports):-
  valid_flight_path([X|Xs]),
  X = [City1, _, _],
  (member(City1, Airports) ->
    fail;
   not(member(City1, Airports))->
    append([City1], Airports, NewAirports)
  ),
  valid_loop_free_path(Xs, NewAirports).

	valid_loop_free_path_helper(_,[]).
	valid_loop_free_path_helper(Flight_leg,[Check|Visited]):-
	    Flight_leg = [_,_,City1],
	    Check = [City2,_,_],
	    (
			 City1 = City2->fail;
			 City1 \= City2
			),
	    valid_loop_free_path_helper(Flight_leg, Visited).


trip(Origin, Destination, Route):-
	update_path(Origin, Destination, Path, []),
	valid_loop_free_path(Path, []),
  price_of_a_path(Path, 0, Price),
  duration_of_a_path(Path, 0, Duration),
  num_of_airlines(Path, 0, NumAirlines, []),
  Route = [Price, Duration, NumAirlines, Path].

update_path(Destination, Destination, [], _).
update_path(Origin, Destination, [Flight_leg|Route], Visited):-
  % (
  %   not(is_list(Route))->Route = [0, 0, 0, []];
  %   is_list(Route)
  % ),
  % last1(Route, Path),
	% Path = [Flight_leg|Rest],
  airport(Origin, _, _),
  airport(Next, _, _),
  (
    flight(Airline, Origin, Next, _, _);
    flight(Airline, Next, Origin, _, _)
  ),
	Flight_leg = [Origin, Airline, Next],
  % (
  %   valid_flight_path(Path)->
	% 		Flight_leg = [Origin, Airline, Next],
  %     append(Path, [Flight_leg], NewPath);
  %   not(valid_flight_path(Path))->
	% 		Flight_leg = [Origin, Airline, Next],
  %     append(Path, [Flight_leg], ListPath),
	% 		list(ListPath, NewPath)
  % ),
	% valid_loop_free_path([Flight_leg|Rest], []),
	valid_loop_free_path_helper(Flight_leg,Visited),
	append(Visited,[Flight_leg],NewVisited),
  update_path(Next, Destination, Route, NewVisited).

tripk(Origin, Destination, K, Route):-
	trip(Origin,Destination,Route),
	Route = [_, Duration, _, _],
	K > Duration.

multicitytrip(Origin, Destination, Intermediate, Route):-
	trip(Origin,Destination,Route),
	last1(Route, Path),
	member([Intermediate, _, _], Path).
