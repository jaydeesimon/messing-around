import scala.io.Source
import scala.util.matching.Regex

case class Circuit(name: String, coordination: Int, endurance: Int, pizazz: Int)
case class Juggler(name: String, coordination: Int, endurance: Int, pizazz: Int, preferredCircuits: Array[String])

object JuggleFest {
  def main(args: Array[String]) = {
    val rawCircuitsAndJugglers = parseRawJuggleInputFile("jugglefest1.txt")
    val circuits = parseCircuits(rawCircuitsAndJugglers("CIRCUIT"))
    val jugglers = parseJugglers(rawCircuitsAndJugglers("JUGGLER"))
  }

  def parseCircuits(circuitLines: Array[String]): Seq[Circuit] = circuitLines.map(parseCircuitLine(_))

  def parseJugglers(jugglerLines: Array[String]): Seq[Juggler] = jugglerLines.map(parseJugglerLine(_))

  def parseRawJuggleInputFile(file: String): Map[String, Array[String]] = {
    val source = Source.fromFile("jugglefest1.txt").mkString.split("\n")
    source.groupBy{ line =>
      line match {
        case l if l.startsWith("C") => "CIRCUIT"
        case l if l.startsWith("J") => "JUGGLER"
        case _ => "GARBAGE"
      }
    }
  }

  def parseCircuitLine(circuitLine: String): Circuit = {
    val regex = new Regex("""C (\w+) H:(\d+) E:(\d+) P:(\d+)""", "name", "h", "e", "p")
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

  def parseJugglerLine(jugglerLine: String): Juggler = {
    val regex = new Regex("""J (\w+) H:(\d+) E:(\d+) P:(\d+) (.+)""", "name", "h", "e", "p", "prefs")
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


