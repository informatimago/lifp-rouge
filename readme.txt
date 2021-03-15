# -*- mode:org;coding:utf-8 -*-

* The Rougelike! Introduction

Current Version: 1.61

The game takes a satirical approach at [[http://en.wikipedia.org][Wikipedia]].  Your character is
a "rouge" admin, and you must commit as many outrageous actions as
possible before you'll get forced out of Wikipedia.  For each such
action you'll get Rouge points.  You also have Karma points, which are
given for good actions and subtracted for bad actions.  When you have
low karma, people would hate you a lot.

** Monsters

| title       | description                                       |
|-------------+---------------------------------------------------|
| noob        | very weak and stupid.  If welcomed, becomes user. |
| user        | standard guy without any special powers.  (*)     |
| vandal      | several types, but all of them are very evil      |
| troll       | attacks people at random                          |
| admin       | can delete stuff and ban users (with banhammer).  |
| bureaucrat  | can promote users to admins.                      |
| Jimbo Wales | just wanders around :)                            |

(*): If user is especially good, he gets promoted to admin.

** Controls

The controls are similar to other roguelike games:

| move around                                      | numpad/arrows |
|--------------------------------------------------+---------------|
| wait                                             | '5'           |
| delete article or userbox                        | 'd'           |
| revert vandalism                                 | 'r'           |
| switch weapon between banhammer & bare hands (*) | 'w'           |
| look around                                      | 'l'           |
| see next message                                 | SPACE         |
| recap previous messages                          | BACKSPACE     |
| welcome n00b                                     | 'W'           |
| quit                                             | 'Q'           |

(*): you wield no weapon by default.  Banhammer doubles changes that
     you get to your stats during combat.

You can edit keybindings.  Open file "controls.cfg" and put a number
that corresponds to the key near the action (I'm sure you can figure
out where to put it).  To discover which number corresponds to which
key, press the key during the game and look at the number between
brackets at the bottom.

* Compiling and running

`rouge` is a curse program.  It needs to be run in a terminal.

#+BEGIN_CODE
(pushd "/path/to/rouge/SRC/" asdf:*central-registry*)
(ql:quickload :rouge)
(rouge:start-game)
#+END_CODE

When working with `slime` in `emacs`, one way to launch `rouge` is to
use =uiop:run-program= to launch =xterm=:

#+BEGIN_CODE
(pushd "/path/to/rouge/SRC/" asdf:*central-registry*)
(ql:quickload :rouge)
(uiop:run-program "xterm -e sbcl \\
     --eval '(progn (push \"/Users/pjb/Downloads/r/rouge/SRC/\" asdf:*central-registry*)
               (ql:quickload :rouge))' \\
     --eval '(unwind-protect (rouge:start-game)
               (sb-ext:quit))'")
#+END_CODE

