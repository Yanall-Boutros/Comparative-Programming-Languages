% $Id: to_upper.pl,v 1.1 2018-12-04 13:47:28-08 - - $
to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).
