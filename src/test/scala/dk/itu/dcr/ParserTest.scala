package dk.itu.dcr

import org.scalatest._
import java.io.PrintWriter
import java.io.File

class ParserTest extends FlatSpec with Matchers {
  val dcrFile = "./mortgage.xml"

  def withFile(testCode: (File, PrintWriter) => Any) {
    val file = File.createTempFile("temp", "xml")
    val writer = new PrintWriter(file)
    try {
      testCode(file, writer)
      } finally {
        writer.close()
        file.delete()
      }
  }

  "A DCR graph" should "be loadable" in {
    assert(DCR.fromXmlFile(dcrFile) !== None)
  }

  it should "be addable" in {
    val dcr = DCR.fromXmlFile(dcrFile)
    assert(dcr !== None)
    assert(!dcr.get.activities.exists(_.id === "id"))
    val newDcr = dcr.get.addActivity(id = "id", name = "name")
    assert(newDcr.activities.exists(_.id === "id"))
  }

  "the dcr to xml" should "be reversable?" in withFile { (file, writer) =>
    val dcr = DCR.fromXmlFile(dcrFile)
    assert(dcr !== None)
    val xml = dcr.get.toXmlString
    writer.write(xml.toString)
    writer.flush()
    val dcr2 = DCR.fromXmlFile(file.getAbsolutePath)
    assert(dcr2 !== None)
    assert(dcr === dcr2)
  }

  "ActivityOps" should "work with include example" in {
    val dcrOpt = DCR.fromXmlFile(dcrFile)
    assert(dcrOpt !== None)
    val dcr = dcrOpt.get
    import dcr.conv
    val activity = dcr.activities.find(_.id == "Statistical appraisal").get
    assert(activity.includes.find(_.id == "Irregular neighbourhood") !== None)
  }

  "Adding relation" should "also add the reverse" in {
    val dcrOpt = DCR.fromXmlFile(dcrFile)
    assert(dcrOpt !== None)
    val dcr = dcrOpt.get
    val id1 = "Statistical appraisal"
    val id2 = "On-site appraisal"
    assert(dcr.conv(dcr.activities.find(_.id == id1).get).includesThis.find(_.id == id2) == None)
    val dcr2 = dcr.addRelation(DCR.Include, dcr.activities.find(_.id == id1).get,
      dcr.activities.find(_.id == id2).get)
    import dcr2._
    assert(dcr2.activities.find(_.id == id2).get.includesThis.find(_.id == id1) !== None)
  }
}
