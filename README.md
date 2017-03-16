
Generate suggestions for assigning volunteers into empty slots
on a rota.

At this point it operates by using the British Museum method to
generate all possible rotas, and then scoring them.  There is a
case to be made for switching to an iterative evolutionary
approach that progresses towards a good-enough goal.  This is
partly to reduce processing time as the problem space increases,
but also to handle making redrafts, by computing the smallest
possible change needed to satisfy updated constraints.

These are some of the properties of the rotas it generates:

* No clashes with unavailability!
* Evenly spread among people
* Not serving too many times in a row
* Not always putting the same people together

These are the properties I want but haven't yet got:

* Avoiding massive gaps in a counter's schedule
* Takes into account service so far on previous rotas
* Handle "can do but prefer not to"
* Ensures enough combined experience

