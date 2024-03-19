import scala.::
import java.io.{PrintWriter, File}
import scala.util.Random


object BernoulliTrials {

  type REAL_RESULT = (Double, Double)
  type RESULTS_2D = List[REAL_RESULT]

  def go = {
    val numTrials = 10
    val fewestFlips = 10
    val mostFlips = 2000
    val r = new Random()
    val results: RESULTS_2D = conductAllTrials(r, numTrials, fewestFlips, mostFlips)
    println(s"results = ${results}")
    val fileName = s"streak_length_for_num_flips_${numTrials}_${fewestFlips}_${mostFlips}.csv"
    val pw = new PrintWriter(new File(fileName))
    results.map(e => pw.write(s"${e._1},${e._2}\n"))
    pw.close()
  }

  def conductAllTrials(r: Random, numTrials: Int, fewestFlips: Int, mostFlips: Int): RESULTS_2D = {
    (fewestFlips to mostFlips).foldLeft(List.empty[REAL_RESULT])({
      case (res: RESULTS_2D, flipsPerTrial) =>
        res.::((flipsPerTrial.toDouble, averageMaxStreakLength(r, numTrials, flipsPerTrial)))
      })
  }

  def averageMaxStreakLength(r: Random, numTrials: Int, flipsPerTrial: Int): Double = 
    (1 to numTrials).foldLeft(0: Int)({
      case (acc, _) => acc + longestStreak(r, flipsPerTrial)
    }).toDouble / numTrials

  def longestStreak(r: Random, numFlips: Int): Int = 
    (1 to numFlips).foldLeft((0: Int, 0: Int, false: Boolean))({
      case ((longestStreak: Int, currStreakLength: Int, headsLastTime: Boolean), _) => if (r.nextBoolean()) {
        if (headsLastTime) {
          if (currStreakLength + 1 > longestStreak) (currStreakLength + 1, currStreakLength + 1, true)
          else (longestStreak, currStreakLength + 1, true)
        } else {
          (longestStreak, 1, true)
        }
      } else {
        (longestStreak, currStreakLength, false)
      }
    })._1
}
