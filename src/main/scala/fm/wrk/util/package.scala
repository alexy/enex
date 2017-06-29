package fm.wrk

/**
  * Created by alexy on 6/17/17.
  */
package object util {
  def readStringMapFromTSV(filename: String, separator: String = "\t") = scala.io.Source.fromFile(filename).getLines()
    .map(_.split(separator)).toList
    .foldLeft(Map[String, String]()) { case (m, a) => m + (a(0) -> a(1))}

  def showTags(tags: List[String]): String = tags.mkString(";")
}
