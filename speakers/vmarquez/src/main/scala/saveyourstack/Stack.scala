package saveyourstack 

import scalaz._
import Scalaz._
import org.joda.time._
import scala.annotation.tailrec

object Dates {
  def generateDates(sd: DateTime, ed: DateTime): List[DateTime] = {
    if (sd isBefore ed)
      sd :: generateDates(sd.plusDays(1), ed)
    else
      List(sd)
  }
  
  def generateDatesSafe(sd: DateTime, ed: DateTime): List[DateTime] = {
    def rec(s: DateTime, l: List[DateTime]): List[DateTime] = 
      if (s isBefore ed)
        rec(s.plusDays(1), s :: l)
      else
       l 
    rec(sd, List())
  }
}

object MyTrampoline {

  implicit val m: Monad[VTrampoline] = new Monad[VTrampoline] {
    def point[A](a: => A): VTrampoline[A] = NoMore(a)

    def bind[A,B](vt: VTrampoline[A])(f: A => VTrampoline[B]): VTrampoline[B] = vt.flatMap(f)
  }
  
  case class More[A,B](a: () => VTrampoline[A], f: A => VTrampoline[B]) extends VTrampoline[B]
  case class NoMore[A](a: A) extends VTrampoline[A]

  trait VTrampoline[A] {
    def flatMap[B](f: A => VTrampoline[B]): VTrampoline[B] = 
      More(() => this, f)
     
    @tailrec 
    final def gogo(): A = this match {
      case NoMore(a) => a
      case More(ta, f) => ta() match {
          case NoMore(a) => f(a).gogo
          case More(tb, ff) => 
            tb().flatMap(b => ff(b).flatMap(f)).gogo
        }
    }
  }
}

object FileSystemStuff {
  
  sealed trait FS
  case class File(s: String) extends FS 
  case class Directory(s: String, l: List[FS]) extends FS {
    override def toString(): String = 
      s + " children size = " + l.size
  }

  def generateFakeFiles(h: Int, w: Int): FS =  {
    def rec(h: Int): FS = h match { 
      case 0 => Directory(h.toString, (0 to w).map(i => File(i.toString)).toList) //we're done
      case 1 => Directory(h.toString, (0 to w).map(_ => rec(h-1)).toList)
      case _ => Directory(h.toString, List(rec(h-1)))
    }
    rec(h)
  }
  
  import scalaz.Free._
  def generateDeepFakeFilesTrampolined(h: Int, w: Int): FS = {
    def rec(h: Int): Trampoline[FS] = h match {
      case 0 => Trampoline.done(Directory(h.toString, (0 to w).map(i => File("filefile")).toList))
      case 1 => (0 to w)
                .map(_ => rec(h -1))
                .toList
                .sequence //sequence goes from F[G[A]] to G[F[A]], so in this case a List[Trampoline[FS]] to Trampoline[List[FS]]
                .map(l => Directory(h.toString, l)) //map on the Trampoline to get access to the thunked recursive call
      case _ => rec(h-1).map(n => Directory(h.toString, List(n)))
    }
    rec(h).run
  }

  def findDepth(f: FS): Int = {
    def rec(files: List[(Int,FS)], s: Set[Int]): Int = files match {
      case (ctr, Directory(n, subfiles)) :: tail => rec(subfiles.map(f => (ctr+1,f)) ::: tail, s + ctr) 
      case (ctr, File(n)) :: tail => rec(tail, s + ctr)
      case _ => s.max 
    }
    rec(List((0,f)), Set())
  }

  def countEntries(f: FS): Int = {
    def rec(ctr: Int, files: List[FS]): Int = files match {
      case Directory(n, subfiles) :: tail => rec(ctr+1, subfiles ::: tail) 
      case File(n) :: tail => rec(ctr, tail)
      case _ => ctr 
    }
    rec(0,List(f))
  }
}

object MonadTransformerProblems {
  import MyTrampoline._

  def handleFiles(): Unit = {
    val a = (0 to 10000).map(ii => State[Int,Int](i => (i,ii)) ).foldLeft( State[Int,Int](i => (i,0)) )( (s,a) => s.flatMap(i => a.map(ii => (ii+i) ))) 
    val b = (0 to 10000).map(ii => StateT[Free.Trampoline,Int,Int](i => Trampoline.done((i,ii))) ).foldLeft( StateT[Free.Trampoline, Int,Int](i => Trampoline.done((i,0))) )( (s,a) => s.flatMap(i => a.map(ii => (ii+i) ))) 
    val c = (0 to 10000).map(ii => JDState[Free.Trampoline,Int,Int](Trampoline.done((i:Int) => Trampoline.done((i,ii)))) ).foldLeft( JDState[Free.Trampoline, Int,Int](Trampoline.done(i => Trampoline.done((i,0)))) )( (s,a) => s.flatMap(i => a)) 
    val e = (0 to 10000).map(ii => liftToJDVT(ii) ).foldLeft( liftToJDVT(1) )( (s,a) => s.flatMap(i => a))
    
    //a and b will fail. c won't
    val res = c.sf.map(i => i(0)).join.run //(0).run //weird to pull out a bit
    
       val d = (0 to 10000).map(ii => State[Int,Int](i => (i,ii)).liftF ).foldLeft( State[Int,Int](i => (i,0)).liftF )( (s,a) => s.flatMap(i => a.map(ii => (ii+i) )))
           val otherResult = d.foldRun(0)( (a,b) => b(a))
  
    (0 to 10000).map(ii => StateT[Free.Trampoline,Int,Int](i => Trampoline.done((i,ii))) ).foldLeft( StateT[Free.Trampoline, Int,Int](i => Trampoline.done((i,0))) )( (s,a) => s.flatMap(i => a.map(ii => (ii+i) ))) 

  }

  def liftToJDVT[A](a: A): JDState[VTrampoline, A, A] = JDState(NoMore(((aa:A) => NoMore((aa,a)))))
  
  //Ok John Degoes had a great idea, if F[] is stack safe and we do all our binding *before* we pass a lambda to the state constructor, we're OK 
  case class JDState[F[_], S, A](sf: F[S => F[(S,A)]]) {
    
    def flatMap[B](f: A => JDState[F, S, B])(implicit M: Monad[F]): JDState[F, S, B] = 
      JDState[F, S, B](((s1: S) => { 
        sf.flatMap(sfa => { 
            println("sfa = " + sfa)
            sfa(s1).flatMap(t =>  
          f(t._2).sf.flatMap(z => z(s1)) 
          )
        }) 
      }).point[F])
  }

}



  
