package algorithm

import java.util.concurrent._

import scala.io.Source
object MyUtils {
  // The fork/join framework uses a work-stealing algorithm
  // http://supertech.csail.mit.edu/papers/steal.pdf
  val forkJoinPool = new ForkJoinPool

  def task[T](computation: => T): RecursiveTask[T] = {
    val t = new RecursiveTask[T] {
      def compute = computation
    }

    Thread.currentThread match {
      case wt: ForkJoinWorkerThread =>
        t.fork() // schedule for execution
      case _ =>
        forkJoinPool.execute(t)
    }

    t
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task { taskB }
    val left = taskA

    (left, right.join())
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
  def readFromFile(path: String): List[(Int, Int)] = {
    var points: List[(Int, Int)] = List()
    for (line <- Source.fromFile(path).getLines()){
      //println(line)
      val new_point = line.split(' ') match {
        case Array(x, y) => (x.toInt, y.toInt)
      }
      points = new_point :: points
    }
    points
  }
}