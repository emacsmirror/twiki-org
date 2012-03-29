twiki-org-mode
==============

This is a half-assed attempt at writing a TWiki/Foswiki Emacs mode
based on Org-mode.  I've taken twiki-outline.el, written by Noah
S. Friedman, as my base.

As of now, cycling visibility with the tab key works.  Hey, it's a
start. :-)  Here's what else works:

* Promoting and demoting items

Released under the GPL v2, like Org and the original twiki-outline.
Share and enjoy!

What's borked (aka TODO):
-------------------------

* Tables -- hitting tab seems to fail.

* Doubtless doing some Emacs stuff quite wrong; this is my first
  attempt at anything like this.

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

* [[http://example.org][Links]] are not displayed as just Links, as
  they are in org-mode.

* Possibly implementing the "Can I get the visibility-cycling features
  in outline-mode and outline-minor-mode?" part of the Org FAQ
  (http://orgmode.org/worg/org-faq.html)



aardvark [at] saintaardvarkthecarpeted [dot] com

March 16, 2012
