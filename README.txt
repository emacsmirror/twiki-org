This is a half-assed attempt at writing a TWiki/Foswiki Emacs mode
based on Org-mode.  I've taken twiki-outline.el, written by Noah
S. Friedman, as my base.

As of now, cycling visibility with the tab key works.  Hey, it's a
start. :-)  Here's what else works:

* Promoting and demoting items (though with occasional confusion, and
  a limit of three headings -- not sure why)

What's borked (aka TODO):

* Tables -- hitting tab seems to fail.
* Doubtless doing some Emacs stuff quite wrong; this is my first
  attempt at anything like this.
* Tags like <verbatim> are hidden, presumably by Org-mode.

Released under the GPL v2, like Org and the original twiki-outline.
Share and enjoy!

aardvark [at] saintaardvarkthecarpeted [dot] com

March 16, 2012
