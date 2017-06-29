package fm.wrk.retag

/**
  * Created by alexy on 6/17/17.
  */
case class Renote(title: String, tags: List[String])

object Renote {
  def loadFileTagged(fileName: String): List[Renote] = scala.io.Source.fromFile(fileName).getLines().toList flatMap { case line =>
    line split ("\t") match {
      case Array(title, tags) =>
        Some(Renote(title, tags.split(";").toList.sorted))
      case _ => None
    }
  }
}

