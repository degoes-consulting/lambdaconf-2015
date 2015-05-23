LambdaConf 2015, Boulder CO, May 23
===================================

Slides and code for my talk "Idiomatic Scala: Your Options Do Not Match".

This presentation is meant to be run inside the Scala REPL.
To view it, download and save the files `LambdaConf2015.scala`
and `LambdaConf2015.txt` to the same folder and proceed as follows:

```sh
$ scala -Dscala.color -language:_ -nowarn -i LambdaConf2015.scala
Loading LambdaConf2015.scala...
defined class LambdaConf2015

Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_45).
Type in expressions to have them evaluated.
Type :help for more information.

scala> val lambdaconf2015 = LambdaConf2015(intp=$intp)
lambdaconf2015: LambdaConf2015 = LambdaConf2015(0,0,LambdaConf2015.txt,false,false,scala.tools.nsc.interpreter.ILoop$ILoopInterpreter@26c8b296)

scala> import lambdaconf2015._
import lambdaconf2015._

scala> h
Usage:
  next          n      >     go to next build/slide
  previous      p      <     go back to previous build/slide
  redraw        z            redraw the current build/slide
  Next          N      >>    go to next slide
  Previous      P      <<    go back to previous slide
  i next        i n          advance i slides
  i previous    i p          go back i slides
  i go          i g          go to slide i
  first         f      |<    go to first slide
  last          l      >|    go to last slide
  Last          L      >>|   go to last build of last slide
  run           r      !!    execute code that appears on slide
  blank         b            blank screen
  help          h      ?     print this help message
scala> f
```

For more information about the presentation tool, visit https://github.com/marconilanna/REPLesent
