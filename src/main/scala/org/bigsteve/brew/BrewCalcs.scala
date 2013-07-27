package org.bigsteve.brew

object BrewCalcs {

  val maltLookup = Map( "2 Row Lager" -> 37.0,
                        "6 Row Base" -> 35.0,
                        "2 Row Pale Ale" -> 38.0,
                        "Biscuit" -> 35.0,
                        "Victory" -> 35.0,
                        "Vienna" -> 35.0,
                        "Munich" -> 35.0,
                        "Brown" -> 32.0,
                        "Dextrin" -> 32.0,
                        "Light Crystal" -> 35.0,
                        "Pale Crystal" -> 34.0,
                        "Medium Crystal" -> 34.0,
                        "Dark Crystal" -> 33.0,
                        "Special B" -> 31.0,
                        "Chocolate" -> 28.0,
                        "Roast Barley" -> 25.0,
                        "Black Patent" -> 25.0,
                        "Wheat" -> 37.0,
                        "Rye" -> 29.0,
                        "Flaked Oats" -> 32.0,
                        "Flaked Corn" -> 39.0,
                        "Flaked Barley" -> 32.0,
                        "Flaked Rice" -> 38.0,
                        "Malto-Dextrin Powder" -> 40.0,
                        "Sugar" -> 46.0 )

  def maxPPG(maltsIn: Map[String,Double]): Double = {
    val total: Double = maltsIn.values.reduce(_ + _)
    val vals = for {
      (i, j) <- maltsIn
    } yield j * maltLookup.getOrElse(i, 0.0)
    vals.reduce(_ + _) / total
  }

  def alcoholContent(og: Double, fg: Double): Double =
    (og - fg) * (1.003 * (og - fg) + 125.65)

  def effCalc(wort: Double, og: Double, malt: Double, mppg: Double): Double = {
    val galWort: Double = wort * 0.264
    val grav: Double = 1000 * (og - 1)
    val lbMalt: Double = 2.2 * malt
    val ppg: Double = galWort * grav / lbMalt
    ppg / mppg
  }

  def strikeWater(l: Double, kg: Double, t1: Double, t2: Double): Double =
    (0.41 / (l / kg)) * (t2 - t1) + t2

  def addWater(l: Double, kg: Double, t1: Double, t2: Double): Double =
    (t2 - t1) * (0.41 * kg + l) / (100 - t2)

  def sgTempCorrection(t: Double): Double =
    -4.544e-3 + t * 1.696e-4 + t * t * 2.806e-6
}
