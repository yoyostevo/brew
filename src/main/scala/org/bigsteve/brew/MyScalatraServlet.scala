package org.bigsteve.brew

import org.scalatra._
import scalate.ScalateSupport

import BrewCalcs._

class MyScalatraServlet extends BrewStack {

  get("/") {
    <html>
      <body>
        <h1>Brew!</h1>
        <ul>
          <li><a href="/brew/water"/>Water calculations</li>
          <li><a href="/brew/efficiency"/>Efficiency calculation</li>
          <li><a href="/brew/alcohol"/>Alcohol calculation</li>
        </ul>  
      </body>
    </html>
  }

  get("/brew/efficiency") {
    contentType="text/html"
    jade("efficiency")
  }

  post("/brew/efficiency") {
    contentType="text/html"
    val malt: Map[String, Double] = (for {
      (i, j) <- maltLookup
      if params(i) != "0"
    } yield (i, params(i).toDouble)).toMap
    val mppg: Double = maxPPG(malt)
    val l: Double = params("wort").toDouble
    val og: Double = params("og").toDouble
    val kg: Double = malt.values.reduce(_ + _)
    jade("efficiency", "calc" -> effCalc(l,og,kg,mppg))
  }

  get("/brew/water") {
    contentType="text/html"
    jade("water")
  }

  post("/brew/water") {
    contentType="text/html"
    val l: Double = params("litres").toDouble
    val kg: Double = params("kilos").toDouble
    val t1: Double = params("starttemp").toDouble
    val t2: Double = params("targettemp").toDouble
    if (params("calctype") == "strike") {
      jade("water", "calc" -> strikeWater(l,kg,t1,t2))
    } else {
      jade("water", "calc" -> addWater(l,kg,t1,t2))
    } 
  }
  
  get("/brew/alcohol") {
    contentType="text/html"
    jade("alcohol")
  }

  post("/brew/alcohol") {
    contentType="text/html"
    val og: Double = params("og").toDouble
    val fg: Double = params("fg").toDouble
    jade("alcohol", "calc" -> alcoholContent(og,fg))
  }
}
