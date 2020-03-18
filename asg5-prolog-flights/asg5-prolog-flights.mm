.so Tmac.mm-etc
.if t .Newcentury-fonts
.SIZE 12 14
.INITR* \n[.F]
.TITLE CSE-112 Winter\~2020 Program\~5 "Logic Programming\(:) Prolog"
.RCS "$Id: asg5-prolog-flights.mm,v 1.14 2020-02-26 17:20:17-08 - - $"
.PWD
.URL
.H 1 "Logic Programming in Prolog"
You will use Prolog to write an airline reservation system for
the Twilight Zone Airlines.
Given a request to travel from one city to another,
print out the flight schedule.
For each leg of the trip,
print out the departure airport, airport, city name, and time.
For example\(::
.DS
.TVCODE* 0 "?- " "fly( lax, sea)."
.TVCODE* 0 "depart  LAX  Los Angeles     14:22"
.TVCODE* 0 "arrive  SFO  San Francisco   15:29"
.TVCODE* 0 "depart  SFO  San Francisco   16:02"
.TVCODE* 0 "arrive  SEA  Seattle-Tacoma  17:42"
.DE
.P
A database has been provided which lists some airports,
their cities,
and the North latitudes and West longitudes of the airports.
A flight schedule has also been provided listing the departure
and arrival airports and the departure times.
.H 1 "Notes about the logic of the program"
.ALX a ()
.LI
To compute the distance between airports,
use the 
haversine formula.
The database contains degrees and minutes,
which must be converted to radians.
The result must be converted to miles.
.LI
Planes fly at a constant speed of 500 miles per hour and 
always using great circle paths,
so the arrival time can be computed from the departure time.
.LI
A flight transfer always takes 30 minutes, 
so during a transfer at a hub,
the departure of a connecting flight must be at least 30
minutes later than the arrival of the incoming flight.
.LI
There are no overnight trips.
The complete trip departs and arrives in the same day,
although the final arrival time may be shown as greater than
23:59.
.LE
.H 1 "Running swipl"
You may load a script by the command
.VTCODE* 1 "swipl -s \f[I]filename.pl\f[P]
or by just running 
.V= swipl
at the command line, 
then loading a module with the Prolog command
.VI [ filename ]
In this latter case, do not include the
.V= \&.pl
suffix in the consult command.
To avoid colorized diagnostics from 
.V= swipl ,
you may put the following line in 
.V= \[Do]HOME/.swiplrc \(::
.VTCODE* 1 ":- set_prolog_flag(color_term,false)."
When searching for Prolog functions, include the string
.V= swipl
in your search to avoid finding advice about other Prolog 
implementations.
.H 1 "What to Submit"
Submit one file\(::
.V= functions.pl .
Also submit
.V= README ,
and if applicable,
.V= PARTNER .
The grader will copy in the files
.V= \&.score/database.pl
and the test data.
