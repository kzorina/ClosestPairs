package algorithm

import algorithm.MyUtils.parallel


object ClosestPair {
  val nWorkers = 2
  def brute_force(array: List[(Int, Int)]) = {
    var min_dis = Double.PositiveInfinity
    for (i <- 0 to array.length - 1)
      for (j <- i+1 to array.length - 1)
        if (Math.sqrt(
          Math.pow(array(i)._1 - array(j)._1, 2) +
            Math.pow(array(i)._2 - array(j)._2, 2)
        ) < min_dis)
          min_dis = Math.sqrt(
            Math.pow(array(i)._1 - array(j)._1, 2) +
              Math.pow(array(i)._2 - array(j)._2, 2)
          )
    min_dis
  }

  def find_closest_split_pair(splitPoints: List[(Int, Int)], delta: Double): (Double, ((Int, Int),(Int, Int))) = {
    if (splitPoints.length < 2)  return (Double.PositiveInfinity, ((splitPoints.head._1, splitPoints.head._2), (splitPoints.head._1, splitPoints.head._2)))
    var min_dis = delta
    var pair = (splitPoints(0), splitPoints(1))
    for (i <- 0 to splitPoints.length - 2)
      for(j <- 1 to Math.min(7, splitPoints.length - i - 1))
        if (Math.sqrt(
                      Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) +
                      Math.pow(splitPoints(i)._2 - splitPoints(i+j)._2, 2)
            ) < min_dis) {
          min_dis = Math.sqrt(Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) + Math.pow(splitPoints(i)._2 - splitPoints(i+j )._2,2))
          pair = (splitPoints(i), splitPoints(i+j))
        }
    (min_dis, pair)
  }

  def find_closest_pair_par(sortedPoints: List[(Int, Int)], nWorkers: Int): (Double, ((Int, Int),(Int, Int))) = {
    var current_point = ((sortedPoints.head._1, sortedPoints.head._2), (sortedPoints.head._1, sortedPoints.head._2))
    var delta = Double.PositiveInfinity
    if (sortedPoints.length < 2) return (delta, current_point)
    val (left_points, right_points) = sortedPoints.splitAt(sortedPoints.length/2)
    val (left, right) = nWorkers match {
      case 1 => (find_closest_pair_par(left_points,nWorkers),find_closest_pair_par(right_points,nWorkers))
      case _ => parallel(find_closest_pair_par(left_points, nWorkers / 2), find_closest_pair_par(right_points, nWorkers / 2))
    }

    if (left._1 < right._1) {
      current_point = left._2
      delta = left._1
    }
    else {
      current_point = right._2
      delta = right._1
    }
    val mean_x = right_points.head._1
    val splitPoints = sortedPoints.filter(_._1 >= mean_x - delta).filter(_._1 <= mean_x + delta).sortBy(_._2)
    val (split, pair) = find_closest_split_pair(splitPoints, delta)
    if (split < delta)
      (split, pair)
    else
      (delta, current_point)
  }


  def main(args: Array[String]): Unit = {
    //var points =  readFromFile("inputPoints.txt") //fill from file
    val z = List.fill(1000)(1000).map(_ => (scala.util.Random.nextInt(30000),scala.util.Random.nextInt(30000))) // random fill
    val sorted_points = z.sortBy(_._1)

    // parallel
    val t1 = System.nanoTime()
    val (distance, point) = find_closest_pair_par(sorted_points, nWorkers)
    val t2 = System.nanoTime()

    // sequensial
    val t3 = System.nanoTime()
    val (distance_seq, point_seq) = find_closest_pair_par(sorted_points, 1)
    val t4 = System.nanoTime()

    val t5 = System.nanoTime()
    val dist = brute_force(sorted_points)
    val t6 = System.nanoTime()

    println("Elapsed time (parallel scan): "+(t2-t1) + " ns")
    println("Elapsed time (sequential scan): "+(t4-t3) + " ns")
    println("Elapsed time (brute): "+(t6-t5) + " ns")

    // we can see, that this algorithm is much faster then brute force

    println("(par) Distance "+distance+" in "+point+" point")
    println("(seq) Distance "+distance_seq+" in "+point_seq+" point")
    println("Brute: "+dist)

  }
}
