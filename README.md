Lambda: The Gathering submission by Magic Missiles
==================================================

Submission to the 2011 ICFP Contest.

Our strategy is in [Brain/Submitted.hs](blob/master/src/Brain/Submitted.hs), it
is relatively straightforward:

In a loop:

  * Find slots of the opponent that have a big expression in them
  * Create a `help` function partially applied to the opponent's slots
  * Prepare a zombie that will send the `help` function
  * Find a dead slot of the opponent
      - if it doesn't exist, attack the opponent's weakest slot
  * Apply the opponent's dead slot to the zombie
  * In between these step, any time we need a free slot of our own,
    revive one of our dead slots

Any time we need a slot number, we sort the numbers by how easy it is the
create it.


Team members
------------
  * Martijn van Steenbergen ([@MedeaMelana](http://twitter.com/MedeaMelana))
  * Sjoerd Visscher ([@sjoerd_visscher](http://twitter.com/sjoerd_visscher))
  * Tom Lokhorst ([@tomlokhorst](http://twitter.com/tomlokhorst))

