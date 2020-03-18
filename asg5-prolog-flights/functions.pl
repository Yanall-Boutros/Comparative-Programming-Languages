% Prolog version of not
% Muchc of thise code is compiled from Professor Wesley Mackey's
% Language Example directory
% the man is a GOAT: Greatest Of All Time
not( X) :- X, !, fail.
not( _).
dmRad(Deg, Min, Rad) :-
    A is Min/60,
    B is A+Deg,
    C is B*pi,
    Rad is C/180.

haversine(Lat1, Lat2, Long1, Long2, Rad, Dist) :-
    abs(Lat2 - Lat1, DP), 
    abs(Long2 - Long1, DL),
    A is (sin(DP/2)**2) + cos(Lat1) * cos(Lat2) * sin(DL/2)**2,
    C is 2 * atan2(sqrt(A), sqrt(1-A)),
    round(abs(Rad * C), Dist).

dist(S, F, D) :-
    airport(S, _, degmin(A,B), degmin(C,Y)),
    airport(F, _, degmin(E,Z), degmin(G,H)),
    dmRad(A,B,Lat1), dmRad(E,Z, Lat2), dmRad(C, Y, Long1),
    dmRad(G, H, Long2),
    haversine(Lat1, Lat2, Long1, Long2, 3961, D).

my_time(S, F, T) :-
    dist(S, F, D),
    T is D/500.

% From graphpaths.pl, change tried to the value in flight
flightpath(Node, End, [flight(Node, Next, Time) | Outlist]) :-
    flight(Node, Next, Time),
    flightpath(Next, End, [flight(Node, Next, Time)], Outlist).
% Base Case
flightpath(Node, Node, _, []).

% From graphpaths.pl, change tried to prvious node vertices, 
% change outlist to next node vertices
flightpath(N, End, [flight(PN1, PN2, time(H1, M1)) | Prev],
          [flight(N, N2, time(H2, M2)) | Current]) :-
    % If othere is a flight from Node to Next at departure time Time
    % and if Next is not a member of tried
    not(N = End),
    % Get flight nodes/vertices
    flight(N, N2, time(H2, M2)),
    % Determine their times appropriately
    my_time(PN1, PN2, T),
    my_time(N, N2, T2),
    % Check if they violate boundary conditions
    T + H1 + (M1/60) + 0.5 =< H2 + (M2/60),
    T2 + H2 + (M2/60) < 24.0,
    % If everything valid then append to Outlist (renamed to Prev)
    Prev_Prime = append( [flight( PN1, PN2, time(H1,M1))],Prev),
    % check to make sure we don't create a cycle.
    not(member(N2, Prev_Prime)),
    not(PN1 = N2),
    not(N2 = PN2),
    flightpath(N2, End, [flight(N, N2, time(H2,M2))| Prev_Prime],
        Current).
% fix for format time
hfix(H, O) :-
    H >= 10,
    number_string(H, Ho),
    string_concat(Ho, "", O).
hfix(H, O) :-
    H < 10,
    number_string(H, Hstring),
    string_concat("0", Hstring, O).
format_time(T, O) :-
    floor(T*60, H),
    Ho is H // 60,
    floor(T * 60, M),
    Mo is M rem 60,
    hfix(Ho, Hstring),
    hfix(Mo, Mstring),
    string_concat(Hstring, ":", Hout),
    string_concat(Hout, Mstring, O).
% wp base case: empty list
writepath([]) :- nl, !.

writepath([flight(S, F, time(H, M)) | T]) :-
    airport(S, D, _, _),
    airport(F, A, _, _),
    Dtime is H + (M/60),
    my_time(S, F, Atime),
    format_time(Dtime, DOut),
    format_time((Atime+Dtime), AOut),
    format("depart     ~w    ~w    ~w~narrive     ~w    ~w    ~w~n",
           [S, D, DOut, F, A, AOut]),
    writepath(T).
fly(S, F) :-
    flightpath(S, F, Out),!,nl,
    writepath(Out).
fly(S, S) :-
    write("no").
fly(_, _) :-
    write("Airport Does Not Exist"), nl,
    write("no").
