:-consult('airlines.pl').

%% trip
print( [ ] ).
print( [ X | Y ] ):- nl, write(X), print( Y ).

trip(Origin, Destination, Route):-
    trip_helper(Origin,Destination,Route,[]).

trip_helper(Destination,Destination,[],_).

trip_helper(Origin,Destination,[Leg|Route],PreviousLeg):-
    airport(Origin,_,_),
    airport(NextPlace,_,_),
    (
		flight(Airline,Origin,NextPlace,_,_);
		flight(Airline,NextPlace,Origin,_,_)
    ),
    Leg = [Origin,Airline,NextPlace],
    no_loop(Leg,PreviousLeg),
    append(PreviousLeg,[Leg],NewPreviousLeg),
    trip_helper(NextPlace,Destination,Route,NewPreviousLeg).

no_loop(_,[]).

no_loop(Leg,[LegNext|Route]):-
    Leg = [_,_,City1],
    LegNext = [City2,_,_],
    City1 \= City2,
    no_loop(Leg,Route).

%% tripk

tripk(Origin, Destination, K, Route):-
	trip(Origin,Destination,Route),
	cal_duration(Route,0,Duration),
	K > Duration.

cal_duration([],Duration,Duration).

cal_duration([Leg|Route],Sum,Duration):-
	Leg = [City1, Airline,City2],
	(
		flight(Airline,City1,City2,_,FlightTime);
		flight(Airline,City2,City1,_,FlightTime)
    ),
	airport(City1,_,DelayTime),
	NewSum is Sum+FlightTime+DelayTime,
	cal_duration(Route,NewSum,Duration).

%% multicitytrip

multicitytrip(Origin, Destination, Intermediate, Route):-
	trip(Origin,Destination,Route),
	memberchk([Intermediate,_,_],Route).

%% Extra Credit

findbesttrips(Origin,Route):-
 	airport(Destination,_,_),
	Destination \= Origin,
	findbesttrips_helper(Origin,Destination,Route).

findbesttrips_helper(Origin,Destination,BestRoute):-
	findall(Route,trip(Origin,Destination,Route),Routes),
	maplist(cal_cost,Routes,Costs),
	find_min_pos(Costs,MinCost),nth0(0,MinCost,MinCostIndex),
	nth0(MinCostIndex,Routes,BestRoute).

cal_cost(Route,Cost):-
	cal_cost(Route,0,Cost).

cal_cost([],Cost,Cost).

cal_cost([Leg|Route],Sum,Cost):-
	Leg = [City1, Airline,City2],
	(
		flight(Airline,City1,City2,FlightCost,_);
		flight(Airline,City2,City1,FlightCost,_)
    ),
	airport(City1,Tax,_),
	NewSum is Sum+FlightCost+Tax,
	cal_cost(Route,NewSum,Cost).

%% via stack overflow
find_min_pos(L,Is) :-
    min_in_list(L,M),
    find_pos(M,L,Is).

find_pos(X,L,Is) :-
    find_pos(0,X,L,Is).

find_pos(_,_,[],[]).
find_pos(I,X,[Y|T],MIT) :-
    (
		X == Y -> MIT = [I|IT];
		MIT=IT
	),
    I1 is I+1,
    find_pos(I1,X,T,IT).

min_in_list([Min],Min).                 % We've found the minimum

min_in_list([H,K|T],M) :-
    H =< K,                             % H is less than or equal to K
    min_in_list([H|T],M).               % so use H

min_in_list([H,K|T],M) :-
    H > K,                              % H is greater than K
    min_in_list([K|T],M).               % so use K
