
object JuggleFest {
  def main(args: Array[String]) = {
    val jugglersAndCircuits = JugglersAndCircuits.fromFile("jugglefest1.txt")
    
    val jugglers = jugglersAndCircuits.jugglers
    val circuits = jugglersAndCircuits.circuits
    jugglersAndCircuits.assignJugglersToCircuits

    // jugglersAndCircuits.jugglersWhoPreferCircuit("C0", 0)

    // for (juggler <- jugglers) {
    //   for (preferredCircuit <- juggler.preferredCircuits.zipWithIndex) {
    //     println(juggler.name + " prefers " + preferredCircuit)
    //   }
    // }
  }

}


