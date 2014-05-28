package pl.szymonmatejczyk.wikipediaanalysis

import scala.io.Source
import scala.xml.pull._
import java.io.File
import java.io.FileOutputStream
import scala.util.matching.Regex

object DumpToGraphConverter extends App {

  val xmlFile = args(0)
  val outputLocation = new File(args(1))

  val out = new FileOutputStream(outputLocation)

  val xml = new XMLEventReader(Source.fromFile(xmlFile))

  var insidePage = false
  var insideTitle = false
  var insideText = false
  var currentPage: Option[String] = None
  var links = collection.mutable.Set[String]()

  val linksRegex = new Regex("""\[\[[^\]]+\]\]""")
  for (event <- xml) {
    event match {
      case EvElemStart(_, "page", _, _) => {
        insidePage = true
      }
      case EvElemEnd(_, "page") => {
        writePage(currentPage.get, links)
        currentPage = None
        links.clear
      }
      case EvElemStart(_, "title", _, _) if insidePage => {
        insideTitle = true
      }
      case EvElemStart(_, "text", _, _) if insidePage => {
        insideText = true
      }
      case EvElemEnd(_, "title") if insideTitle => {
        insideTitle = false
      }
      case EvElemEnd(_, "text") if insideText => {
        insideText = false
      }
      case EvText(t) => {
        if (insideTitle) {
          currentPage = Some(t)
        }
        if (insideText) {
          val linksInLine = linksRegex.findAllIn(t).toList
          val pattern = """\A\[\[(.+)\]\]\Z""".r
          linksInLine.foreach {
            case pattern(name) =>
              links += name
          }
        }
      }
      case _ => // ignore
    }
  }

  def writePage(pageName: String, links: collection.Set[String]) = {
    println(pageName + " " + links.size)
    print(links.mkString("", "\n", "\n"))
  }

  out.close()

}