package fm.wrk.enex

import com.lucidchart.open.xtract.{XmlReader, __}
import com.lucidchart.open.xtract.XmlReader._

import play.api.libs.functional.syntax._


/**
  * Created by alexy on 6/17/17.
  */

case class Enex (
  exportDate: String,
  notes: Notes
) {
  override def toString: String = {
    val ns =
      notes.notes map { case Note(title, tags) =>
        s"$title\t" + tags.mkString(";")
      }
    //s"export date: $exportDate\n" +
    ns.mkString("\n")
  }
  def taggedOnly: Enex = {
    val taggedNotes = notes.notes.filterNot(_.tags.isEmpty)
    copy(notes=notes.copy(notes=taggedNotes))
  }
}

object Enex {
  implicit val reader: XmlReader[Enex] = (
    attribute[String]("export-date") and
    __.read[Notes] // the root tag, en-export, is __, we never have to mention its name explicitly
    )(apply _)
}

case class Notes(notes: Seq[Note])

object Notes {
  implicit val reader: XmlReader[Notes] = (__ \ "note").read(seq[Note]).map(apply _)
}

case class Note(title: String, tags: Seq[String])

object Note {
  implicit val reader: XmlReader[Note] = (
    (__ \ "title").read[String] and
      (__ \ "tag").read(seq[String])
    )(apply _)
}