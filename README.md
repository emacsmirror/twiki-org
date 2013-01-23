twiki-org-mode
==============

TWiki/Foswiki and Orgmode.  Like chocolate and peanut butter.

Ahem.

This is a half-assed attempt at writing a TWiki/Foswiki Emacs mode
based on Org-mode.  I've taken twiki-outline.el, written by Noah
S. Friedman, as my base.

As of now, cycling visibility with the tab key works.  Hey, it's a
start. :-)  Here's what else works:

* Promoting and demoting items

* When hitting alt-enter on a list item (3 spaces then asterisk),
  you'll get another list item

* Indenting a list item with twiki-org-demote-item
  will move it in by a multiple of 3

* [[http://example.org][Links]] are displayed as just Links, as they
  are in org-mode.

* Saving buffers works even when longlines-mode is enabled.  (Really
  oughta start a proper changelog...)

What's borked (aka TODO):
-------------------------

* Tables -- hitting tab seems to fail.

* Tags like <verbatim> are hidden, presumably by Org-mode.

  This turns out to be because the original mode hides html tags by
  default; I didn't notice that. (<blush>) For now I've bound <f9> to
  twiki-org-toggle-html, which brings up its own set of bugs:

  - This should be configurable

  - For some reason, the toggling won't happen until I press <f9> and
    then any other key.  It happens immediately if I run M-x
    twiki-org-toggle-html. (Whoops: turns out I had <f9>-y bound to
    something, so evidently it was waiting for <f9> to be followed by
    something else to make sure it wasn't that function.  But now, if
    I bind it to f8, it doesn't toggle until I switch to another
    buffer.)

* Possibly implementing the "Can I get the visibility-cycling features
  in outline-mode and outline-minor-mode?" part of the Org FAQ
  (http://orgmode.org/worg/org-faq.html)

* shift-left/right on a list item promotes the headline it's in,
  rather than indenting the list item.

* alt-right will demote a list *and* the heading it's in; alt-left
  will just demote the heading.

* Doubtless doing some Emacs stuff quite wrong; this is my first
  attempt at anything like this.

License
-------

Released under the GPL v3, like Orgmode.  Comments, patches and
suggestions welcome.  Share and enjoy!

Contact
-------

aardvark [at] saintaardvarkthecarpeted [dot] com

Fork, etc
---------

* https://github.com/saintaardvark/twiki-org
* http://git.saintaardvarkthecarpeted.com/?p=twiki-org.git;a=summary

March 16, 2012
