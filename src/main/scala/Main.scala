import java.io.File

import scala.io.Source
import scala.util.Random
import scala.xml.{Elem, XML}

case class Document(id: Long, header: String, text: String) {
  def toXml: Elem = <doc><field name="id">{id}</field><field name="header">{header}</field><field name="text">{text}</field></doc>
}

object Main {
  val documentDelimiter: String = "\\*{2,}"
  val random: Random = Random
  var maxIdSoFar = 0L
  val MAX_ID = 6004L
  val randomRange = 100

  def iterateDocument(fileId: Long): List[String] => Option[Document] = {
    case document: List[String] if document.isEmpty =>
      None
    case document: List[String] =>
      val headLine: String = document.head
      val id = headLine match {
        case hl if hl.startsWith("Document") =>
          val idTemp = hl.split("\\s+")(1).toLong
          idTemp match {
            case idTemp1 if idTemp1 > maxIdSoFar =>
              maxIdSoFar = idTemp1
              idTemp1
            case _ => (idTemp.toString + 1.toString).toLong
          }
        case _ => MAX_ID + random.nextInt(randomRange)
      }
      val header = document match {
        case rol if rol.exists(_.matches("\\s*")) => rol.drop(1).takeWhile(!_.matches("\\s*")).mkString(" ")
        case _ => ""
      }
      val text = header match {
        case "" => document.mkString(" ")
        case _ => document.dropWhile(!_.matches("\\s*")).drop(1).mkString(" ")
      }
      Some(Document((fileId.toString + id.toString).toLong, header, text))
  }

  def iterate(file: List[String]): List[List[String]] =
    file match {
      case file: List[String] if file.isEmpty =>
        Nil
      case file: List[String] =>
        val document = file.takeWhile(!_.matches(documentDelimiter))
        document :: iterate(file.dropWhile(!_.matches(documentDelimiter)).drop(1))
    }

  def produceXmlFromFile: String => List[Elem] = fileName => {
    val file = Source.fromResource("partitions/" + fileName).getLines.toList
    val fileId = fileName.toSeq match {
      case Seq('L', 'I', 'S', 'A', x, '.', y @ _*) => Seq(x, y).mkString("").toLong
      case _ => 0
    }
    iterate(file).map(iterateDocument(fileId)).filter(_.isDefined).map(_.get).map(_.toXml)
  }

  def main(args: Array[String]): Unit = {
    val fileNames = new File("src/main/resources/partitions").listFiles.filter(_.isFile).toList.map(_.getName).sorted
    val a = <add>{fileNames.flatMap(produceXmlFromFile)}</add>
    XML.save("src/main/resources/lisa.xml", a, "utf-8", xmlDecl = false, null)
  }
}

