% $Id: formatting.pl,v 1.4 2020-03-06 14:37:44-08 - - $
% Formatting output in swipl.

pad2( Num, Padded) :- Num < 10, string_concat( '0', Num, Padded).
pad2( Num, Num).

print_flight( Activity, Code, Name, Hour, Min) :-
    write( Activity), write( '  '),
    string_upper( Code, Upper),
    write( Upper), write( ' '), write( Name), write( ' '),
    pad2( Hour, Hour2), write( Hour2), write( ':'),
    pad2( Min, Min2), write( Min2), nl.

test_print :-
    print_flight( 'depart', lax, 'Los Angeles     ', 14, 22),
    print_flight( 'arrive', sfo, 'San Francisco   ', 15, 29),
    print_flight( 'depart', sfo, 'San Francisco   ', 16, 02),
    print_flight( 'arrive', sea, 'Seattle-Tacoma  ', 17, 42).

