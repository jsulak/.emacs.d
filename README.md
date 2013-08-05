# My Emacs settings

An ever-changing set of Emacs settings. 

## Setup

To grab all the dependencies:

    git clone --recursive git://github.com/jsulak/.emacs.d.git

The first time you start Emacs, it will install some additional packages
that are best handled by the package manager.

## Install Emacs on mac

I use [Emacs For Mac OS X](http://emacsformacosx.com).

## Tips for using these Emacs settings

If you want to use my settings straight out of the box, here are some things to note:

 * Read Steve Yegge's [Effective Emacs](https://sites.google.com/site/steveyegge2/effective-emacs).

 * Start by reading up on all the cool stuff in james-bindings.el.

 * Add your machine-specific stuff in ~/.emacs.d/custom.el.

 * `C-h` is rebound to backspace, like in the shell. Get help on `F1` instead.

 * expand-region is your friend. Find its bound key by doing `C-h f er/expand-region`

 * Undo with `C-z` or `C-_` and redo with `M-_`. Watch the undo-tree with `C-x u`

 * I recommend rebinding Caps Lock to Ctrl and use that instead of the often badly placed Ctrl-key.

 * Watch [emacsrocks.com](http://emacsrocks.com).


## Survival guide for the first week of Emacs

When you start using Emacs for the first time, your habits fight you every inch
of the way. Your fingers long for the good old familiar key bindings. Here's an
overview of the most commonly used shortcuts to get you through this pain:

* `C      ` Shorthand for the ctrl-key
* `M      ` Shorthand for the meta-key (alt-key)
* `S      ` Shorthand for the shift-key

### Files

* `C-x C-f` Open a file. Starts in the current directory
* `C-x C-s` Save this file
* `C-x C-w` Save as ...
* `C-x b  ` Switch to another open file (buffer)
* `C-x C-b` Switch to another open file in a new window
* `F5     ` Revert buffer

### Cut copy and paste

* `C-space` Start marking stuff. C-g to cancel.
* `C-x C-k` Cut (aka kill) (kills entire line if no selection)
* `C-k    ` Cut till end of line
* `C-w    ` Kill word forward
* `C-q    ` Kill word backward
* `M-w    ` Copy (copies entire line if no selection)
* `C-y    ` Paste (aka yank)
* `M-y    ` Cycle last paste through previous kills
* `C-x C-y` Choose what to paste from previous kills XXX
* `C-'    ` Mark stuff quickly. Press multiple times

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` or `C-x C-m` Run a command by name
* `C-z    ` Undo
* `M-_    ` Redo
* `C-x u  ` Show the undo-tree
* `C-c o  ` Occur-mode - show all occurrences of string in file.
* `C-x m  ` Open magit. It's a magical git interface for Emacs

### Navigation

* `C-arrow` Move past words/paragraphs
* `C-a    ` Go to indentation, then start of line
* `C-e    ` Go to end of line
* `C-c C-g` Go to line number
* `M-.    ` Go to tag
* `C-.    ` Jump back from tag
* `C-s    ` Search forward. Press `C-s` again to go further.
* `C-r    ` Search backward. Press `C-r` again to go further.

### Window management

* `C-x 0  ` Close this window
* `C-x 1  ` Close other windows
* `C-x 2  ` Split window horizontally
* `C-x 3  ` Split window vertically
* `S-arrow` Jump to window to the left/right/up/down

### Help

* `C-h t   ` Basic tutorial
* `C-h k   ` Help for a key binding
* `C-h r   ` Emacs' extensive documentation

## Other Emacs configs

Almost everything in my Emacs config was borrowed (stolen) from someone else's.  Exploring other people's configs are one of the best ways to learn about how Emacs works.  Some good sources:

* [Magnar's .emacs.d](https://github.com/magnars/.emacs.d) (much of this text is borrowed from here)
* [milkypostman's dotemacs](https://github.com/milkypostman/dotemacs)
* [purcell's emacs.d](https://github.com/purcell/emacs.d)
* [Emacs Starter Kit](https://github.com/technomancy/emacs-starter-kit)

