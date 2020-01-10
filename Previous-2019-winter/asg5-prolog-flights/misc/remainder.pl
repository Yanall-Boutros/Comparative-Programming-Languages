% $Id: remainder.pl,v 1.1 2019-03-13 14:09:03-07 - - $

remainder( Float, Modulus, Quotient, Remainder) :-
   Quotient is truncate( Float / Modulus),
   Remainder is Float - Quotient * Modulus.

/*

remainder( 93.468, 60, Q, R).

remainder( 918.62849, 60, Q, R).

*/
