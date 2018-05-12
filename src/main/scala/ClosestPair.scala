import MyUtils._
object ClosestPair {
  val nWorkers = 2
  def brute_force() = {}
  def find_closest_split_pair(splitPoints: List[(Int, Int)], delta: Double): (Double, ((Int, Int),(Int, Int))) = {
    println("Split for:   " + splitPoints)
    var min_dis = delta
    var pair = (splitPoints(0), splitPoints(1))
    for (i <- 0 to splitPoints.length - 2)
      for(j <- 1 to Math.min(7, splitPoints.length - i - 1))
        if (Math.sqrt(
                      Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) +
                      Math.pow(splitPoints(i)._2 - splitPoints(i+j)._2, 2)
            ) < min_dis) {
          println("curr min: "+min_dis)
          println("("+splitPoints(i)._1+" - "+splitPoints(i+j)._1+")^2+("+splitPoints(i)._2+" - "+splitPoints(i+j)._2+")^2 = "
            +Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) +" + "+
            Math.pow(splitPoints(i)._2 - splitPoints(i+j)._2, 2))
          min_dis = Math.sqrt(Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) + Math.pow(splitPoints(i)._2 - splitPoints(i+j )._2,2))
          pair = (splitPoints(i), splitPoints(i+j))
        }
    println("Split distance: " + min_dis)
    (min_dis, pair)
  }
  def find_closest_pair_par(sortedPoints: List[(Int, Int)], nWorkers: Int): (Double, ((Int, Int),(Int, Int))) = {
    //println("Worker count: " + nWorkers)
    println("sorted list: "+ sortedPoints)
    if (sortedPoints.length < 2) return (Double.PositiveInfinity, ((sortedPoints.head._1, sortedPoints.head._2), (sortedPoints.head._1, sortedPoints.head._2)))
    val (left_points, right_points) = sortedPoints.splitAt(sortedPoints.length/2)
    println("Left: "+left_points)
    println("Right: "+right_points)
    val (left, right) = nWorkers match {
      /*case 1 => (find_closest_pair_par(left_points,nWorkers),find_closest_pair_par(right_points,nWorkers))
      case _ => parallel(find_closest_pair_par(left_points, nWorkers / 2), find_closest_pair_par(right_points, nWorkers / 2))
      */
        // TO RUN NON PARALLEL
      case _ => (find_closest_pair_par(left_points,nWorkers),find_closest_pair_par(right_points,nWorkers))


    }
    /*
    if (nWorkers >= 2) {
       parallel(find_closest_pair_par(left_points, nWorkers / 2), find_closest_pair_par(right_points, nWorkers / 2))
    }
    else {
      val (left, right) = (find_closest_pair_par(left_points,nWorkers),find_closest_pair_par(right_points,nWorkers))
    }*/


    // if left < right - curr point = left_2
    val delta = Math.min(left._1, right._1)
    println("Delta: " + delta)
    val mean_x = right_points.head._1
    println("Mean x: "+ mean_x)
    //println("Seeking in (" + (mean_x - delta) + ", " + (mean_x + delta) + ")")
    //println("No filter"+sortedPoints)
    //println("First filter"+sortedPoints.filter(_._1 >= mean_x - delta))
    //println("Second filter"+sortedPoints.filter(_._1 >= mean_x - delta).filter(_._1 <= mean_x + delta))
    val splitPoints = sortedPoints.filter(_._1 >= mean_x - delta).filter(_._1 <= mean_x + delta)
    if (splitPoints.length < 2)  return (Double.PositiveInfinity, ((sortedPoints.head._1, sortedPoints.head._2), (sortedPoints.head._1, sortedPoints.head._2)))
    val (split, pair) = find_closest_split_pair(splitPoints, delta)
    //println(pair)
    println("min("+delta+", "+split+") = " + Math.min(delta, split))
    (Math.min(delta, split),pair)

  }


  def main(args: Array[String]): Unit = {
    val points = readFromFile("inputPoints.txt")
    println(points)
    val sorted_points = points.sortBy(_._1)
    println(sorted_points)


    // parallel
    val distance = find_closest_pair_par(sorted_points, nWorkers)
    println(distance)

  }
}
