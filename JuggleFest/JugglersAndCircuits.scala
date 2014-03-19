import scala.io.Source
import scala.util.matching.Regex
import JugglersAndCircuits.{ CIRCUIT, JUGGLER }

case class Circuit(name: String, coordination: Int, endurance: Int, pizazz: Int)
case class Juggler(name: String, coordination: Int, endurance: Int, pizazz: Int, preferredCircuits: Array[String])

class JugglersAndCircuits(val file: String) {

  private val rawJugglerAndCircuitLines: Map[String, Array[String]] = {
    val source = Source.fromFile(file).mkString.split("\n")
    source.groupBy { line =>
      line match {
        case l if l.startsWith("C") => CIRCUIT
        case l if l.startsWith("J") => JUGGLER
        case _ => "GARBAGE"
      }
    }
  }

  lazy val jugglers: Array[Juggler] = {
    val regex = new Regex("""J (\w+) H:(\d+) E:(\d+) P:(\d+) (.+)""", "name", "h", "e", "p", "prefs")
    val jugglerLines = rawJugglerAndCircuitLines(JUGGLER)
    jugglerLines.map { jugglerLine =>
      regex.findFirstMatchIn(jugglerLine) match {
        case Some(parsedLine) =>
          Juggler(
            parsedLine.group("name"),
            parsedLine.group("h").toInt,
            parsedLine.group("e").toInt,
            parsedLine.group("p").toInt,
            parsedLine.group("prefs").split(",")
          )
        case None => throw new RuntimeException("Unparseable juggler line: " + jugglerLine)
      }
    }
  }

  lazy val circuits: Array[Circuit] = {
    val regex = new Regex("""C (\w+) H:(\d+) E:(\d+) P:(\d+)""", "name", "h", "e", "p")
    val circuitLines = rawJugglerAndCircuitLines(CIRCUIT)
    circuitLines.map { circuitLine =>
      regex.findFirstMatchIn(circuitLine) match {
        case Some(parsedLine) => 
          Circuit(
            parsedLine.group("name"),
            parsedLine.group("h").toInt,
            parsedLine.group("e").toInt,
            parsedLine.group("p").toInt
          )
        case None => throw new RuntimeException("Unparseable circuit line: " + circuitLine)
      }
    }
  }

  lazy val circuitMap: Map[String, Circuit] = {
    circuits.foldLeft(Map[String, Circuit]())((map, circuit) => map + (circuit.name -> circuit))
  }

  lazy val circuitPreferenceMap: Map[Tuple2[String, Int], Seq[String]] = {
    var circuitPrefMap = scala.collection.mutable.Map[Tuple2[String, Int], Seq[String]]()
    for (juggler <- jugglers) {
      val preferredCircuitsWithIndex = juggler.preferredCircuits.zipWithIndex
      preferredCircuitsWithIndex.foreach { circuitNamePrefTuple =>
        val currentCircuitNames = circuitPrefMap.getOrElse(circuitNamePrefTuple, Seq())
        val updatedCircuitNames = currentCircuitNames :+ juggler.name
        circuitPrefMap.put(circuitNamePrefTuple, updatedCircuitNames)
      }
    }
    circuitPrefMap.toMap
  }

  def assignJugglersToCircuits = {
    val keys = {
      (for (circuit <- circuits; preference <- 0 until circuits.length)
        yield (circuit.name, preference)
      ).sortWith( (lhs, rhs) => lhs._2 < rhs._2)
    }

    keys.foreach { key =>
      println(key + ": " + circuitPreferenceMap(key))
    }

     // Score each juggler in the tier
      // Sort from highest to lowest
      // Take the first (jugglers / circuits)
      // Assign them to the circuit
    
  }
}

object JugglersAndCircuits {
  def fromFile(file: String) = new JugglersAndCircuits(file)

  val CIRCUIT: String = "CIRCUIT"
  val JUGGLER: String = "JUGGLER"
}