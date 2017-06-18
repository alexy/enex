package fm.wrk.enex

/**
  * Created by alexy on 6/17/17.
  */

import com.lucidchart.open.xtract.{ParseSuccess, XmlReader}

import scala.xml.XML

object Main {

  //private val xml = XML.load(getClass.getResourceAsStream("/enex3.xml"))

  def main(args: Array[String]): Unit = {
    val xml = XML.loadFile(args(0))
    val parsedEnex = XmlReader.of[Enex].read(xml)
    //println("Parsed Result:")
   parsedEnex match {
     case ParseSuccess(s) => println(s.taggedOnly)
     case _ => sys.exit(1)
   }
  }
}
