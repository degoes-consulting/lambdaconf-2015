README
======

This repository has the materials for a 2-hour workshop on
[Programming with Refinement Types](http://www.refinement-types.org)
which is also a tutorial introduction to [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell).


Participating
-------------
    
You can run this code in *any of* the following ways:

1. [Online](http://ucsd-progsys.github.io/lh-workshop/)

2. [Virtual Machine](http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova)

3. [Build LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197)

The **online web demo** is easiest for the workshop. See below 


Option 1: Online
----------------

This is the easiest by far; point your browser [here](http://ucsd-progsys.github.io/lh-workshop/)


Option 2: Virtual Machine
-------------------------

This is also very easy, if you can manage the 2Gb download.

**Step 1** Download [this VM image](http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova)

he code files are in `lh-workshop/src/*.lhs`

**Step 2** Choose your editor. For *emacs* do:

       tar -zxvf liquid-emacs.tgz
       
and for *Spacemacs* (a great Vim-Emacs hybrid) do:

       tar -zxvf liquid-spacemacs.tgz

**Step 3** The code files are in

       ~/lh-workshop/src/*.lhs


Option 3: Local Build
---------------------

Finally, if you prefer, you can build LiquidHaskell from:

1. [cabal](https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197)

2. [github](https://github.com/ucsd-progsys/liquidhaskell/#how-to-clone-build-and-install)

And then run `liquid` on [these files](src/) 
