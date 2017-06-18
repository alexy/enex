package fm.wrk.evernote

import com.evernote.auth.{EvernoteAuth, EvernoteService}
import com.evernote.clients.{ClientFactory, NoteStoreClient}
import com.evernote.edam.`type`.{Note, Notebook, Tag}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}
import com.evernote.edam.notestore.NoteFilter
import fm.wrk.evernote.`this`.{RichNote,tagsOrNil}
import fm.wrk.retag.Renote

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import fm.wrk.util._

package object `this` {

  def tagsOrNil(note: Note): List[String] =
    Option(note.getTagNames()) match {
      case Some(tags) => tags.toList
      case _ => Nil
  }

  implicit class RichNote(val note: Note) extends AnyVal {
    def title = note.getTitle()
    def tags: List[String] = tagsOrNil(note)
  }
}

/**
  * Created by alexy on 6/17/17.
  */

case class TaggedNotebooks(notebooks: List[RichNotebook]) {
  override def toString: String = {
    notebooks map (_.toString) mkString("-----\n")
  }
}

case class PoorNote(title: String, tags: List[String], note: Note)

case class RichNotebook(name: String, titles: List[String], tags: List[String],
                        notes: List[RichNote], // List[PoorNote]
                        nb: Notebook) {
  override def toString: String = {
    s"Notebook Name: $name\n\ttitles:\n\t\t" +
    titles.mkString("\n\t\t") +
    "\ttags:\n\t\t" +
    tags.mkString("\n\t\t")
  }
}

private[evernote] object TaggedNotebooks {
  val env = "prod" // "dev" or "prod"

  val developerToken: String = readStringMapFromTSV("devtoken.tsv")(env)

  println("devtoken: " + developerToken)


    // Set up the NoteStore client
  private val evernoteAuth = new EvernoteAuth(EvernoteService.PRODUCTION, developerToken)
  val factory = new ClientFactory(evernoteAuth)
  val noteStore = factory.createNoteStoreClient()
  val allTags = noteStore.listTags().toList.map { case t => (t.getName, t) }.toMap

  // Make API calls, passing the developer token as the authenticationToken param
  val notebookList = noteStore.listNotebooks().toList
  //    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

  //    val ourNotebook = noteStore.getDefaultNotebook
  val notebooks = notebookList.map { case nb => (nb.getName, nb) }.toMap

  def retrieve(notebookNames: List[String]): TaggedNotebooks = {
    val processNotebooks = notebooks.filterKeys(notebookNames.contains(_))

    val richNotebooks: List[RichNotebook] = processNotebooks.toList map { case (nbName, nb) =>

      println("Processing Notebook: " + nb.getName)
      assert(nbName == nb.getName)

      val noteFilterOurNotebook = new NoteFilter()
      noteFilterOurNotebook.setNotebookGuid(nb.getGuid)

      val notes: List[Note] = noteStore.findNotes(noteFilterOurNotebook, 0, 1000).getNotes.toList
      val titles = notes map (_.getTitle()) toList

      //val nbNotes: Map[String, Note] = noteList.map { case note => (note.getTitle, note) }.toMap

//      val nbTags = noteStore.listTagsByNotebook(nb.getGuid).toList.map { case t => (t.getName, t) }.toMap
      val tags: List[String] = noteStore.listTagsByNotebook(nb.getGuid).toList.map (_.getName)


       val fatNotes = notes map (new RichNote(_)) // why List[Note] does not autoconvert to List[RichNote]
      //val fatNotes = notes map { case n => PoorNote(n.getTitle(), tagsOrNil(n), n) }
      RichNotebook(nbName, titles, tags, fatNotes , nb)
    }
    TaggedNotebooks(richNotebooks)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val nbs = TaggedNotebooks.retrieve(List("sbtb2016","DBTB"))
    val renotes: Map[String, Seq[String]] = Renote.loadFileTagged(args(0)) map { case Renote(title, tags) =>
      (title, tags)
    } toMap

    nbs.notebooks foreach { case RichNotebook(nbName, titles, tags, notes, nb) =>
      notes.zipWithIndex foreach { case (rn, i) =>
        try {
          if (renotes.contains(rn.title)) {
            val wereTags = renotes(rn.title).mkString(";")
            val nowTags  = rn.tags.mkString(";")
            println(s"nb [$nbName] title [${rn.title}] tags were: [$wereTags], now: [$nowTags]")
          }
        } catch {
            case _ => println(s"ERROR notebook $nbName, note #$i")
        }
      }
    }
  }
}
