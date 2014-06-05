package pl.szymonmatejczyk.wikipediaanalysis

import scala.io.Source
import scala.xml.pull._
import java.io.{File, FileOutputStream}
import scala.util.matching.Regex
import com.twitter.app.Flags

class DumpToGraphConverter(inputFilename: String, outputFilename: Option[String]) {

  def apply() {
    var out: Option[FileOutputStream] = None

    outputFilename match {
      case Some(filename) => out = Some(new FileOutputStream(filename))
      case None => out = None
    }

    val xml = new XMLEventReader(Source.fromFile(inputFilename))

    var insidePage = false
    var insideTitle = false
    var insideText = false
    var currentPage: Option[String] = None
    var links = collection.mutable.Set[String]()

    val linksRegex = new Regex( """\[\[[^\]]+\]\]""")
    for (event <- xml) {
      event match {
        case EvElemStart(_, "page", _, _) => {
          insidePage = true
          currentPage = Some("title not set")
        }
        case EvElemEnd(_, "page") => {
          writePage(currentPage.get, links)
          currentPage = None
          links.clear()
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
        case EvText(t) =>
          if (insideTitle) {
            currentPage = Some(t)
          }
          if (insideText) {
            val linksInLine = linksRegex.findAllIn(t).toList
            val pattern = """\A\[\[(.+)\]\]\Z""".r
            linksInLine.foreach {
              case pattern(name) =>
                links += name
              case _ => ()
            }
          }
        case _ => // ignore
      }
    }

    def writePage(pageName: String, links: collection.Set[String]) = {
      out match {
        case Some(o) =>
          o.write((pageName + " " + links.size).getBytes())
          o.write(links.map(x => x.replace(" ", "_")).mkString("\n", "\n", "\n").getBytes())
          o.flush()
        case None =>
          print(pageName + " " + links.size)
          print(links.map(x => x.replace(" ", "_")).mkString("\n", "\n", "\n"))
      }
    }

    out match {
      case Some(o) => o.close()
      case None => ()
    }
  }

}

object DumpToGraphConverter extends App {

  val flags = new Flags("Graph generation benchmarks")
  val fileFlag = flags[String]("f", "Filename of a single file to read from")
  val outputFlag = flags[String]("o", "Output filename to write to")
  val directoryFlag = flags[String]("d", "Directory to read all xml files from")
  val helpFlag = flags("h", false, "Print usage")
  flags.parse(args)

  if (helpFlag()) {
    println(flags.usage)
  } else {
    directoryFlag.get match {
      case Some(dirName) =>
        val dir = new File(dirName)
        val filesInDir = dir.list()
        if (filesInDir == null) {
          throw new Exception("Current directory is " + System.getProperty("user.dir") +
            " and nothing was found in dir " + dir)
        }
        if (filesInDir.isEmpty) {
          println("WARNING: empty directory.")
        }
        filesInDir.filter(file => file.endsWith("xml"))foreach {
          file => convertFile(dirName + file, dirName + file.replace(".xml", ".graph"))
        }
      case None =>
        convertFile(fileFlag(), outputFlag())
    }
  }

  def convertFile(inputFilename: String, outputFilename: String) {
    println("Converting file: %s...".format(inputFilename))
    new DumpToGraphConverter(inputFilename, Some(outputFilename))()
  }
}