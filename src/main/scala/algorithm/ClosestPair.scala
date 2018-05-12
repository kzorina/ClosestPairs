package algorithm

import algorithm.MyUtils.{parallel, readFromFile}


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
          min_dis = (Math.sqrt(
            Math.pow(array(i)._1 - array(j)._1, 2) +
              Math.pow(array(i)._2 - array(j)._2, 2)
          ))
    min_dis
  }
  
  def find_closest_split_pair(splitPoints: List[(Int, Int)], delta: Double): (Double, ((Int, Int),(Int, Int))) = {
    //println("Split for:   " + splitPoints)
    if (splitPoints.length < 2)  return (Double.PositiveInfinity, ((splitPoints.head._1, splitPoints.head._2), (splitPoints.head._1, splitPoints.head._2)))
    //val splitPoints = splitPointsIn.sortBy(_._2)
    var min_dis = delta
    var pair = (splitPoints(0), splitPoints(1))
    for (i <- 0 to splitPoints.length - 2)
      for(j <- 1 to Math.min(7, splitPoints.length - i - 1))
        if (Math.sqrt(
                      Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) +
                      Math.pow(splitPoints(i)._2 - splitPoints(i+j)._2, 2)
            ) < min_dis) {
          /*println("curr min: "+min_dis)
          println("("+splitPoints(i)._1+" - "+splitPoints(i+j)._1+")^2+("+splitPoints(i)._2+" - "+splitPoints(i+j)._2+")^2 = "
            +Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) +" + "+
            Math.pow(splitPoints(i)._2 - splitPoints(i+j)._2, 2))*/
          min_dis = Math.sqrt(Math.pow(splitPoints(i)._1 - splitPoints(i+j)._1, 2) + Math.pow(splitPoints(i)._2 - splitPoints(i+j )._2,2))
          pair = (splitPoints(i), splitPoints(i+j))
        }
    //println("Split distance: " + min_dis)
    (min_dis, pair)
  }
  def find_closest_pair_par(sortedPoints: List[(Int, Int)], nWorkers: Int): (Double, ((Int, Int),(Int, Int))) = {
    //println("Worker count: " + nWorkers)
    //println("sorted list: "+ sortedPoints)
    var current_point = ((sortedPoints.head._1, sortedPoints.head._2), (sortedPoints.head._1, sortedPoints.head._2))
    var delta = Double.PositiveInfinity
    if (sortedPoints.length < 2) return (delta, current_point)
    val (left_points, right_points) = sortedPoints.splitAt(sortedPoints.length/2)
    //println("Left: "+left_points)
    //println("Right: "+right_points)
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
    //println("Delta: " + delta)
    val mean_x = right_points.head._1
    //println("Mean x: "+ mean_x)

    val splitPoints = sortedPoints.filter(_._1 >= mean_x - delta).filter(_._1 <= mean_x + delta).sortBy(_._2)
    val (split, pair) = find_closest_split_pair(splitPoints, delta)
    //println(pair)
    //println("min("+delta+", "+split+") = " + Math.min(delta, split))
    if (split < delta)
      (split, pair)
    else
      (delta, current_point)

  }


  def main(args: Array[String]): Unit = {
    var points =  readFromFile("inputPoints.txt")
    val numPoints = 10000
    val r = scala.util.Random
    for (i <- 0 to numPoints){
      points.patch(i,List((r.nextInt(200),r.nextInt(200))),1)
    }
    //val points = readFromFile("inputPoints.txt")
    println("Length "+points.length)

    val z = List.fill(1000)(1000).map(_ => (scala.util.Random.nextInt(20000),scala.util.Random.nextInt(20000)))
    println(z)
    val sorted_points = z.sortBy(_._1)
    //points_sort_y = z.sortBy(_._2)

    //initialize scalameter

    /*val standardConfig = config(
      Key.exec.minWarmupRuns -> 100,
      Key.exec.maxWarmupRuns -> 300,
      Key.exec.benchRuns -> 100,
      Key.verbose -> false) withWarmer new scalameter.Warmer.Default
*/

    // parallel
    val t1 = System.nanoTime()
    val (distance, point) = find_closest_pair_par(sorted_points, nWorkers)
    val t2 = System.nanoTime()

    // sequensial
    val t3 = System.nanoTime()
    val (distance_seq, point_seq) = find_closest_pair_par(sorted_points, 1)
    val t4 = System.nanoTime()

    println("Distance "+distance+" in "+point+" point")
    println("Distance "+distance_seq+" in "+point_seq+" point")

    val t5 = System.nanoTime()
    val dist = brute_force(sorted_points)
    val t6 = System.nanoTime()
    println("Elapsed time (parallel scan): "+(t2-t1) + " ns")
    println("Elapsed time (sequential scan): "+(t4-t3) + " ns")
    println("Elapsed time (brute): "+(t6-t5) + " ns")
    println("Brute: "+brute_force(sorted_points))

  }
}
