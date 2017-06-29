package fm.wrk.evernote

import com.evernote.auth.{EvernoteAuth, EvernoteService}
import com.evernote.clients.{ClientFactory, NoteStoreClient}
import com.evernote.edam.`type`.{Note, Notebook, Tag}
import com.evernote.edam.error.{EDAMNotFoundException, EDAMUserException}
import com.evernote.edam.notestore.{NoteFilter, NoteStore}
import fm.wrk.evernote.TaggedNotebooks.noteStore
import fm.wrk.evernote.`this`.{NoteTagSet, NoteTagSets, NoteTitle, NotebookName, RichNote, TagList, TagMap, TagName, TagNameList, TagNameSet, tagsOrNil}
import fm.wrk.retag.Renote

import scala.collection.JavaConversions._
import fm.wrk.util._

package object `this` {

  type NotebookName = String
  type NoteTitle = String
  type TagName = String

  type TagList = List[Tag]
  type TagNameList = List[TagName]
  type TagNameSet  = Set[TagName]
  type TagMap = Map[TagName, Tag]

  type NoteTagSet = (NoteTitle, TagNameSet)
  type NoteTagSets = List[NoteTagSet]

  // BUG?  note.getTagNames returns empty list even for current notes with tags
  // we can't use it, or RichNote
  // using noteStore.getNoteTagNames instead below, with PoorNote
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

case class TaggedNotebooks(notebooks: List[RichNotebook], allTags: TagMap, noteStore: NoteStoreClient) {
  override def toString: String = {
    notebooks map (_.toString) mkString("-----\n")
  }
}

case class PoorNote(title: String, tags: List[String], note: Note)

case class RichNotebook(name: String, titles: List[String], tags: List[String],
                        notes: List[PoorNote], // List[PoorNote]
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
  val notebookList = noteStore.listNotebooks().toList.sortBy(_.getName) //drop(50).take(10)
  //    notebooks foreach { case nb => println("Notebook: " + nb.getName)}

  //    val ourNotebook = noteStore.getDefaultNotebook
  val notebooks = notebookList.map { case nb => (nb.getName, nb) }.toMap

  def retrieve(notebookNames: List[String]): TaggedNotebooks = {
    val processNotebooks =
      if (notebookNames.isEmpty) notebooks
      else notebooks.filterKeys(notebookNames.contains(_))

    val richNotebooks: List[RichNotebook] = processNotebooks.toList map { case (nbName, nb) =>

      println("Processing Notebook: " + nb.getName)
      assert(nbName == nb.getName)

      val noteFilterOurNotebook = new NoteFilter()
      noteFilterOurNotebook.setNotebookGuid(nb.getGuid)

      val notes: List[Note] = noteStore.findNotes(noteFilterOurNotebook, 0, 1000).getNotes.toList
      val titles = notes map (_.getTitle()) toList

      //val nbNotes: Map[String, Note] = noteList.map { case note => (note.getTitle, note) }.toMap

      //      val nbTags = noteStore.listTagsByNotebook(nb.getGuid).toList.map { case t => (t.getName, t) }.toMap
      val tags: List[String] = noteStore.listTagsByNotebook(nb.getGuid).toList.map(_.getName).sorted


      // val fatNotes = notes map (new RichNote(_)) // why List[Note] does not autoconvert to List[RichNote]
      val fatNotes = notes map { case n =>
        val tags = noteStore.getNoteTagNames(n.getGuid()).toList
        PoorNote(n.getTitle(), tags, n)
      }

//      val fatTags = (fatNotes map (_.tags.toSet) filterNot(_.isEmpty)) match {
//        case Nil => Set()
//        case sets => sets reduce (_++_)
//      }

      val fatTags: Set[String] = fatNotes map (_.tags.toSet) filterNot(_.isEmpty) reduceLeftOption(_++_) getOrElse(Set())

      println(s"notebook [$nbName], tags: [$tags], fattags: [$fatTags]")

      RichNotebook(nbName, titles, tags, fatNotes, nb)
    }
    TaggedNotebooks(richNotebooks, allTags, noteStore)
  }
}

object Main {
  def compareTags(title: NoteTitle, were: TagNameList, now: TagNameList): Option[NoteTagSet] = {
    val addTags: Set[String] = were.toSet -- now.toSet

    if (addTags.isEmpty) None
    else Some(title, addTags)
  }

  def main(args: Array[String]): Unit = {
//    val nbs = TaggedNotebooks.retrieve(Nil)
      val nbs = TaggedNotebooks.retrieve(List("Scala", "setup", "Data By the Bay"))
//    val nbs = TaggedNotebooks.retrieve(List("Penultimate"))
//    val nbs = TaggedNotebooks.retrieve(List("sbtb2016", "DBTB"))
    val renotes: Map[String, TagNameList] = Renote.loadFileTagged(args(0)) map { case Renote(title, tags) =>
      (title, tags)
    } toMap


    val reallyTag = true

    def addTags(note: Note, tags: List[Tag], noteStore: NoteStoreClient, really: Boolean) = {
      print(s"adding tags to note [${note.getTitle}]: ")
      tags foreach { case t =>
        if (really) {
          note.addToTagNames(t.getName)
          print("  !")
        }
        else print(" ~")
        print(t.getName)
      }
      if (really) noteStore.updateNote(note)
      println()
    }


    val (diff, allTags) = nbs.notebooks.foldLeft((Nil: List[(NotebookName, NoteTagSets)], nbs.allTags)) {
      case (nbAcc @ (nbNoteTags, aTags), RichNotebook(nbName, titles, tags, notes, nb)) =>

        val (addNotesTags, allTags): (NoteTagSets, TagMap) = notes.foldLeft((Nil: NoteTagSets, aTags)) {
          case (notesAcc@(nts, aTags: TagMap), rn) if renotes.contains(rn.title) =>
            val wereTags = renotes(rn.title)
            val nowTags = rn.tags

            compareTags(rn.title, wereTags, nowTags) match {
              case Some(noteTags@(title, addTagNames)) =>
                println(s"nb [$nbName] title [${rn.title}] tags were: [$wereTags], now: [$nowTags]")

                val (newNoteTags: TagList, bTags: TagMap) =
                  addTagNames.foldLeft((Nil: TagList, aTags)) {
                    case (theTags@(newTags: TagList, cTags: TagMap), tagName: TagName) =>
                      cTags.get(tagName) match {
                        case Some(t: Tag) =>
                          (newTags :+ t, cTags)
                        case _ => // create a new tag
                          println(s"creating tag $tagName")
                          if (reallyTag) {
                            val t = new Tag()
                            t.setName(tagName)
                            nbs.noteStore.createTag(t)
                            println(s"CREATED new tag ${t.getName}")
                            (newTags :+ t, cTags + (tagName -> t))
                          } else {
                            println(s"FAKING CREATION of the new tag $tagName")
                            theTags
                          }
                      }
                  }
                // now add all the tags, existing as Tag objects by now, missing from the note
                addTags(rn.note, newNoteTags, nbs.noteStore, reallyTag)

                (nts :+ noteTags, bTags)

              case _ =>
                notesAcc
            }

          case (notesAcc, _) => notesAcc
        }

        if (addNotesTags.isEmpty) nbAcc else (nbNoteTags :+ (nbName, addNotesTags), allTags)
    }

    //
    val nbTags = diff.foldLeft(Nil: List[(NotebookName, TagNameSet)]) {
      case (acc, (nbName, addNotesTags)) if addNotesTags.nonEmpty =>
        acc :+ (nbName, addNotesTags map (_._2) reduce(_++_))
      case (acc, _) => acc
    }

    nbTags foreach { case (nbName, tags) =>
      println(s"notebook [$nbName], missing tags: [${showTags(tags.toList)}]")
    }
  }
}
