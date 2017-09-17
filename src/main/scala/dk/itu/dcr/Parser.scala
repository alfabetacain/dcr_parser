package dk.itu.dcr

import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import scala.language.implicitConversions

object Main {
  def main(args: Array[String]): Unit = { 
    val xml = XML.loadFile(args(0))
    val stuff = for {
      events <- (xml \\ "events").headOption.map(_ \ "event")
      labels <- (xml \\ "labels").headOption.map(_ \ "label")
      labelMappings <- (xml \\  "labelMappings").headOption.map(_ \ "labelMapping")
      constraints <- (xml \\ "constraints").headOption
      runtime <- (xml \\ "runtime").headOption
      res <- DCR.buildDCR(events, labels, labelMappings, constraints, runtime)
    } yield res
    println(stuff)
  }
}

object DCR {

  sealed trait Constraint
  case object Condition extends Constraint
  case object Response extends Constraint
  case object Include extends Constraint
  case object Exclude extends Constraint

  def buildDCR(events: NodeSeq, labels: NodeSeq, labelMappings: NodeSeq,
    constraints: NodeSeq, runtime: NodeSeq): Option[DCR] = {
      val eventIds = events.map(_.attribute("id").get.head.toString)
      val labelIds = labels.map(_.attribute("id").get.head.toString)
      val event2label = labelMappings.map(
        x => (x.attribute("eventId").get.head.toString, x.attribute("labelId").get.head.toString)).toMap
      val eventsInfo: Seq[(String, String)] = eventIds.map(e => (e, event2label(e))).seq
      val cons = for {
        conditions <- (constraints \ "conditions").headOption.map(_ \ "condition").orElse(Some(NodeSeq.Empty))
        c <- Some(conditions.map(x => (Condition, extractSourceAndTarget(x))))

        responses <- (constraints \ "responses").headOption.map(_ \ "response").orElse(Some(NodeSeq.Empty))
        r <- Some(responses.map(x => (Response, extractSourceAndTarget(x))))

        includes <- (constraints \ "includes").headOption.map(_ \ "include").orElse(Some(NodeSeq.Empty))
        i <- Some(includes.map(x => (Include, extractSourceAndTarget(x))))

        excludes <- (constraints \ "excludes").headOption.map(_ \ "exclude").orElse(Some(NodeSeq.Empty))
        e <- Some(excludes.map(x => (Exclude, extractSourceAndTarget(x))))
      } yield (c,r,i,e)

      val marking = (runtime \ "marking").head
      val included: Set[String] = ((marking \ "included").head \ "event").map(getId).toSet
      val pending = ((marking \ "pendingResponses").head \ "event").map(getId).toSet
      val executed = ((marking \ "executed").head \ "event").map(getId).toSet
      val status = eventsInfo.map(_._1).map{ id => 
        id -> Map(
          "included" -> (included contains id),
          "pending" -> (pending contains id),
          "executed" -> (executed contains id)
        )
      }.toMap
      //val cons = conditions.map(extractSourceAndTarget(_)).map((Condition, _))
      val res = buildDCR(eventsInfo, cons.map(x => x._1 ++ x._2 ++ x._3 ++ x._4).getOrElse(List.empty), status)
      val other: DCR = new DCR {
        def myMap: DCR#Activity => this.Activity = x => x.asInstanceOf[this.Activity]
        def activities = res.activities.map(myMap)
      }
      val blah = other.activities
      import res.conv
      assert(res.constraints.get(Include) != None)
      val haha: Set[res.Activity] = (res.constraints.get(Include).get.keys.head).includes
      //println(s">>>>>>>>>>>>>>>>>>>>>>>>>>>> $haha")
      Some(res)
  }

  def getId(node: Node): String = node.attribute("id").get.toString

  def buildDCR(events: Seq[(String, String)], cs: Seq[(Constraint, (String, String))],
    runtime: Map[String, Map[String, Boolean]]): DCR = {
    new DCR {
      private def getOrDefault(id: String, att: String, runtime: Map[String, Map[String, Boolean]]): Boolean =
        runtime.get(id).flatMap(_.get(att)).getOrElse(false)
      val activities = events.map{x => 
        ActivityImpl(x._1, x._2, 
          included = getOrDefault(x._1, "included", runtime),
          executed = getOrDefault(x._1, "executed", runtime),
          pending = getOrDefault(x._1, "pending", runtime)
          )
      }
      val map = activities.map(x => x.id -> x).toMap
      override val constraints: Map[DCR.Constraint, Map[this.Activity, Set[this.Activity]]] = 
        cs.groupBy(_._1).map(x => (x._1 -> x._2.foldLeft[Map[this.Activity, Set[this.Activity]]](Map.empty){ case (acc, (_, (act1, act2))) =>
          val a1 = map.get(act1).get
          val a2 = map.get(act2).get
          if (acc contains a1)
            acc + (a1 -> (acc.get(a1).get + a2))
          else
            acc + (a1 -> Set(a2))
        }))

      case class ActivityImpl(id: String, name: String, included: Boolean, executed: Boolean, pending: Boolean) extends Activity
      override def toString = activities.mkString("\n")
    }
  }

  private def extractSourceAndTarget(node: Node): (String, String) = {
    (node.attributes("sourceId").head.toString, node.attribute("targetId").get.head.toString)
  }

  def typeMap(dcr: DCR): DCR#Activity => dcr.Activity = _.asInstanceOf[dcr.Activity]
}

trait DCR { 
  trait Activity {
    def id: String
    def name: String
    def included: Boolean
    def excluded: Boolean = !included
    def pending: Boolean
    def executed: Boolean
  }

  def constraints: Map[DCR.Constraint, Map[this.Activity, Set[Activity]]] = Map.empty

  def activities: Seq[Activity]

  implicit def conv(act: this.Activity): ActivityOps = new ActivityOps(act)
  case class ActivityOps(act: this.Activity) {
    def includes: Set[Activity] = constraints.get(DCR.Include).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def excludes: Set[Activity] = constraints.get(DCR.Exclude).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def responses: Set[Activity] = constraints.get(DCR.Response).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def conditions: Set[Activity] = constraints.get(DCR.Condition).flatMap(_.get(act))
      .getOrElse(Set.empty)
  }

  def addRelation(typ: DCR.Constraint, source: this.Activity, target: this.Activity): DCR = {
    val me = this
    new DCR {
      def activities = me.activities.map(_.asInstanceOf[this.Activity])
      override val constraints = Map(typ -> (Map(source.asInstanceOf[this.Activity] -> Set(target.asInstanceOf[this.Activity]))))
    }
  }
}
