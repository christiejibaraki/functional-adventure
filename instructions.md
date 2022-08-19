# Finishing Up the Code

It's time to finish up your final project!  Isn't it exciting?
There's just a tiny bit of code to write to get the game to run, then
the rest involves adding some features.

As always, you should start by copying your `functional-adventure/src`
directory over from your previous assignment, in this case
Assignment 8.  You may need to update your `package.yaml` so as to
include all third-party libraries you may want to use in this project.

While we're on the topic of third-party libraries, you are not
required to use anything beyond what the homework assignments asked
you to use.  However, if you want to use this project as an
opportunity to explore new third-party libraries, we ask that you
confine your explorations to libraries that are available in the
[Stackage repository, LTS 18.28
resolver](https://www.stackage.org/lts-18.28) .

## The Main Module

To get the game to actually run, we need a `Main` executable
module.  This module has been sitting there in your `stack` project
all quarter, so all you have to do is open it up at `app/Main.hs`.
It should look like this:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, world!"
```

Go ahead and add these imports:

```haskell
import Control.Monad.State

import GameState
import GameIO
```

### Exercise: `main`

It's the end of the line!  Time to define the `main` value and
have an app you can actually run, already.  The `main` value
should do two things:

-   open by printing the message "Welcome to Functional Adventure!"
-   run `repl` from the `GameIO` module in an infinite loop, using
    `initialState` as an initial state

The code for this is quite short, so don't go crazy or anything.  Once
you've defined `main`, you'll be able to actually run *Functional
Adventure* in the shell and play it!

```
$ stack exec functional-adventure-exe
Welcome to Functional Adventure!
-> look
You are in a small kitchen.
You see the following objects:
pot
stove
There are exits in the following directions:
north
east
south
-> take pot and north
You take the pot.
You go north.
-> east
There is no exit in that direction.
-> exit
Goodbye!
```

# Finalize the Minimal Core

Over the course of eight homework assignments, we've had you write a
minimally functional adventure game, with the simplest possible
manifestation of some basic features a text adventure game might be
expected to have.  It isn't necessarily the most fun game of all time
yet (though soon, it will be!) but it is playable and should hopefully
be pretty bug-free.

In this next section of the final project spec, we'll spell out the
features this minimal core is expected to have.

## Ensure All Minimal Core Features Are There

Let's be explicit about what the user should be able to do while
playing the game at this point, starting from what the game looks like
as of Assignment 8.

1.  the user should be able to walk to any room on the map and also
    retrace their steps
2.  there should be enough items in the universe that the user can
    find at least three to walk around and carry, and they should
    have sufficiently different weights that the user can exceed the
    weight limit of the player and eventually fail to keep picking
    more up
3.  the user should be able to move items around between the rooms
    in the game map&#x2014;that is, they should be able to pick one item
    up from Room A, deposit it in Room B, and find that that state
    of the game persists even as they move around and do other
    things
4.  there should be some sequence of moves the user can perform&#x2014;at
    least 3 or 4&#x2014;to win the game
5.  the user should be able to look in each room and see a
    description of the room, a list of the objects in it, and a list
    of the room's exits
6.  the game map should have the shape of the map laid out in
    Assignment 2
7.  taking and dropping items should follow the logic laid out in
    Assignment 6
8.  the parser should interpret commands following the specification
    laid out in Assignment 7
9.  the game should perform whatever commands the user types in
    following the guidelines laid out in Assignment 8

This stuff should basically already be in place from your homework, so
this is mainly just a matter of giving everything a once-over and
performing whatever touch-ups seem appropriate.

Once you have confirmed that these basic features are all working,
it's time to get started on the main part of this final project spec,
which is making the game your own!

# Feature Additions

In this section, we'll describe some additional features we'd like
you to add to your final project.

## Customize the Content

We had you write dummy content for the game while you were
developing it on your homework.  Now's your chance to get creative
and replace it with something more interesting than beans and pots.
You don't have to go crazy here&#x2014;just feel free to go into
`src/Room.hs`, `src/Item.hs`, etc. and personalize the content of
the game in whatever way is most fun without being too
time-consuming.  (Find/replace in your code editor will be your
friend here.)  Maybe the setting is Middle Earth, or Star Trek, or
18th century revolutionary France, or your kindergarten classroom,
or whatever!

## Feature Of Your Choice

This part is a bit more open-ended than what you've been doing in
the class up until now, but hopefully that just means it'll give
you the chance to flex your creative muscles.

We'd like you to add one nontrivial feature to the game.  Nothing
too nontrivial, because you only have a week.  But something
interesting enough to make you think a bit about some of the design
decisions we've been discussing this quarter.

If you don't want to try to think of a feature to add, you can
choose any one feature off of this list:

-   add a new type of command (for instance, you could add the ability
    to eat an object in your inventory, which might make something
    happen, or the ability to teleport to a new room, provided you
    have the right object in your inventory, or the ability to look at
    an object in your inventory and have the game print a description
    of the object)
-   one interesting such command possibility would be to create a
    command with a ditransitive verb in it (such as "use key with
    lock") that allows the player to combine objects, or use something
    in their inventory on some object in a room&#x2014;or any other
    additional command that would prompt you to write interesting new
    parsing code
-   add a new design feature to the player that manifests somehow in
    gameplay (could be a hunger or energy rating, or a way to increase
    the player's strength)
-   create a way for the player to die, ending the game prematurely
-   create a lantern, and make some of the rooms dark (i.e. you can't
    see anything in them, or maybe you can't see all of the exits, or
    maybe you just can't enter them) unless you walk in with the
    lantern
-   add another constraint on taking or dropping (maybe some objects
    are bolted down, and you need do something to loosen them) and
    incorporate it into your validation logic for `takeItem` or
    `dropItem`
-   have the game keep track of the number of moves the user has made
    and make something happen once there have been a certain number of
    moves (maybe the universe changes somehow, or the player dies of
    hunger)

As mentioned above, you are welcome and encouraged to think up your
own feature to add&#x2014;I think that would be a lot of fun both for
you and for us.  But if you go that route, please run it by us either
during office hours or on Ed Discussion, so we can make sure it isn't
either over-or-under-ambitious for a one-week project.

We will also be around to help you spitball ideas about how to
implement whatever additional feature you end up going with during
office hours.

Please note that with any of these features, it's easy to get stuck
down a time-eating rabbit hole if you end up approaching them in the
wrong way.  So it's good to be prepared for that possibility in
advance.  I recommend having a fallback feature-of-your-choice that
you're going to plan to implement if the first one you try
implementing ends up taking too long.

Also, we'd really like you to have fun implementing your own feature,
so it might be a good idea to make sure to give yourself time to do
this part before implementing the final feature, described in the next
section.

## Multiple Levels

The last feature we're going to ask you to implement is to give the
game multiple levels.  On previous assignments, we've walked you
through the design process, recommending ways of splitting the game
code up into modules, suggesting helper functions to simplify your
later code, and laying out the data structures the game is going to
use.  Your job, thus far, has just been to implement that design.

For the final project, as sort of a send-off, we'd like you to get
your feet wet making some big design decisions.  The first part of
that was implementing your own feature.  For this second and final
part, rather than laying out every last little function you have to
implement, we'll describe the feature addition we'd like you to make,
and describe our recommended approach to implementing that feature at
a high level in English, leaving the nitty gritty of how to code
everything up to you.

The task is going to be to give the game multiple levels, where levels
are understood as follows.  In our starter version of the game, the
goal is to pick up the jug and take it into the backyard.  You could
call it a 'one-mission' game; finish the mission, and you've won the
game.  A multiple-levels game, by contrast, will have two missions in
it.  When the user completes the first mission, rather than the game
printing a victory message and exiting, it will move to a new level,
which will involve a new mission.  When that mission is completed, the
game will print a victory message telling the user they've won.

What we'd like you to do when the game changes levels is have the map
change when the level changes.  Storywise, that could be for any
number of reasons.  Maybe in the story of the game, the player took a
plane to a new location.  Or maybe night fell, and the pirate ship the
player was on reached the harbor.  Or perhaps the game is set in a
mansion, and when the first mission is complete, a bunch of secret
passageways to new rooms open up.  More specifically, the level change
should do one of the following two things to the map:

-   transport the player to a completely new game map, consisting of
    different rooms with different exits and different objects
-   modify/extend the original map with new exits leading to new rooms
    with new objects

Regardless of which of these two options you pursue, the new map
should have between 3 and 5 rooms that didn't exist in the starting
map, each of which contain objects that didn't exist in the starting
map, have new descriptions, different exits, and so forth.  So the
"level one" map should have five rooms, and the "level two" map should
have between 3 and 5 new rooms.

When the user completes the first mission, the game should print some
message explaining what is happening in the storyline of the game that
corresponds to the level change, and then the user should be in the
second level until the game ends.  When the user completes the second
mission, the game should exit with a victory message the same way our
starter version did.

# Final Prep

## Clean Up The Code

Please do the following, to present your work in its best light:

1.  remove any unused module imports from your code
2.  remove any unused values from your code (like, for example, if we
    had you define a helper function on the homework that you never
    ended up using)
3.  make sure every function has a toplevel type signature and a
    one-line comment explaining what the function does
4.  break up any lines that exceed 80 characters in length

## Runtime Bugs

You'll also want to check the game for runtime bugs:

1.  make sure every action in the game follows the logic it's supposed
    to
    -   e.g. if the user takes an item, then checks their inventory,
        then looks around, the item should be in their inventory and
        not in the room
    -   or: if the user (successfully) goes north and then goes south,
        they should end up back in the room they came from
    -   make sure all messages that get displayed are getting
        displayed in the right circumstances&#x2014;e.g. if the pot is
        in the room the player is in, it shouldn't print an "I don't
        understand that" error message
    -   the game shouldn't continue after the user wins
    -   etc.&#x2014;things like that
2.  the game shouldn't throw any exceptions&#x2014;every weird
    behavior on the part of the user should be anticipated and
    elegantly handled when the user plays the game

Most of this functionality should already be present from the code you
wrote on your homework assignments, so this is just a matter of
double-checking.

## Readme

Finally, please include a short readme file called
`functional-adventure/README.txt` (you could also make it `.md` or
`.org` if you prefer).  The instructions you are currently reading
happen to be a in a file called `functional-adventure/README.md`, so
to make everything a bit neater, please also rename this file to
`functional-adventure/instructions.md`:

Your readme file should contain:

-   your name
-   the date
-   acknowledgments (i.e. the names of any other students or
    non-students you discussed your project with)
-   instructions for us grading and playing through the game that
    explain what your new feature is, how it works, and how to make
    all the required things happen (e.g. "if you pick up both the
    bowling ball and the backpack, then you won't be able to pick
    anything else up" or "to make the player die, go into the dungeon
    and drink the poison" or "to win the game, drop the sword in the
    throne room")
-   an explanation of any new module(s) that may have ended up in
    your project, if there was/were any, and why you decided to put
    the code for it/them into a new module
-   a description of any runtime bugs that you couldn't figure out
    how to eliminate, if there ended up being any, and how to trigger
    them

Part of what you'll be graded on is how helpful this readme file is as
documentation; think about it like you're a professional app developer
getting ready to ship a product, and these are instructions for your
clients.

# Submitting

Before submitting your project, check to make sure your project and
the files in it look about like this:

```
$ tree
.
├── app
│   └── Main.hs
├── functional-adventure.cabal
├── instructions.md
├── package.yaml
├── README.txt
├── src
│   ├── Command.hs
│   ├── Direction.hs
│   ├── Example.hs
│   ├── GameIO.hs
│   ├── GameState.hs
│   ├── Item.hs
│   ├── Main.hs
│   ├── Player.hs
│   └── Room.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs

3 directories, 17 files
```

This project structure is required for full credit, though we
understand that the `src` directory may also contain several
additional Haskell modules, depending on which features you added.

Have fun with the project!
