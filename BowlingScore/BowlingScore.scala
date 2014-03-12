import scala.io.Source

object BowlingScore {
  def main(args: Array[String]): Unit = {
    val frames = new Frames("input.txt")
    val score = frames.score
    println(score)
  }
}

class Frames(file: String) {
  val frames: Seq[Frame] = parseFrameFile(file)

  def score: Int = {
    frames.foldLeft(0)((totalScore, frame) => totalScore + scoreFrame(frame.ordinal))
  }

  private def scoreFrame(frameIdx: Int): Int = {
    val frame = frames(frameIdx)

    if (frame.isStrike)
      frame.openFrameScore + nextRollScore(frameIdx, 2)
    else if (frame.isSpare)
      frame.openFrameScore + nextRollScore(frameIdx, 1)
    else
      frame.openFrameScore
  }

  private def nextRollScore(fromFrameIdx: Int, numRollsToLookAhead: Int) = {
    val nextFrameScores = frameScores(fromFrameIdx + 1)
    val nextNextFrameScores = frameScores(fromFrameIdx + 2)
    val possibleNextFrameScores = Seq(nextFrameScores, nextNextFrameScores).flatten.grouped(numRollsToLookAhead)

    possibleNextFrameScores.toSeq.headOption.foldLeft(0)((sum, scores) => sum + scores.sum)
  }

  private def frameScores(frameIdx: Int): Seq[Int] = {
    if (frames.isDefinedAt(frameIdx))
      frames(frameIdx).scores
    else
      Seq()
  }

  private def parseFrameFile(file: String): Seq[Frame] = {
    val rawInputSplit = Source.fromFile(file).mkString.split("\n")
    rawInputSplit.zipWithIndex.map(toFrame(_)).toSeq    
  }

  private def toFrame(rawFrame: Tuple2[String, Int]) =
    Frame(rawFrame._2, rawFrame._1.split(" ").map(_.toInt))
}

case class Frame(ordinal: Int, scores: Seq[Int]) {
  def isStrike: Boolean = scores(0) == 10
  def isSpare: Boolean = scores.sum == 10
  def openFrameScore: Int = scores.sum
}
