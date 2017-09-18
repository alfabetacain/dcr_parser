package dk.itu.dcr

import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.Node
import scala.language.implicitConversions
import scala.util.Try

object DCR {

  sealed trait Constraint
  case object Condition extends Constraint
  case object Response extends Constraint
  case object Include extends Constraint
  case object Exclude extends Constraint

  def fromXmlFile(file: String): Option[DCR] = { 
    for {
      xml <- Try(XML.loadFile(file)).toOption
      events <- (xml \\ "events").headOption.map(_ \ "event")
      labels <- (xml \\ "labels").headOption.map(_ \ "label")
      labelMappings <- (xml \\  "labelMappings").headOption.map(_ \ "labelMapping")
      constraints <- (xml \\ "constraints").headOption
      runtime <- (xml \\ "runtime").headOption
      res <- DCR.buildDCR(events, labels, labelMappings, constraints, runtime)
    } yield res
  }

  def buildDCR(events: NodeSeq, labels: NodeSeq, labelMappings: NodeSeq,
    constraints: NodeSeq, runtime: NodeSeq): Option[DCR] = {
      val eventIds = events.map(_.attribute("id").get.head.toString)
      val labelIds = labels.map(_.attribute("id").get.head.toString)
      val event2label = labelMappings.map(
        x => (x.attribute("eventId").get.head.toString, x.attribute("labelId").get.head.toString)).toMap
      val eventsInfo: Seq[(String, String)] = eventIds.map(e => (e, event2label(e))).seq
      val cons = for {
        conditions <- (constraints \ "conditions").headOption
          .map(_ \ "condition").orElse(Some(NodeSeq.Empty))
        c <- Some(conditions.map(x => (Condition, extractSourceAndTarget(x))))

        responses <- (constraints \ "responses").headOption
          .map(_ \ "response").orElse(Some(NodeSeq.Empty))
        r <- Some(responses.map(x => (Response, extractSourceAndTarget(x))))

        includes <- (constraints \ "includes").headOption
          .map(_ \ "include").orElse(Some(NodeSeq.Empty))
        i <- Some(includes.map(x => (Include, extractSourceAndTarget(x))))

        excludes <- (constraints \ "excludes").headOption
          .map(_ \ "exclude").orElse(Some(NodeSeq.Empty))
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
      buildDCR(eventsInfo, 
        cons.map(x => x._1 ++ x._2 ++ x._3 ++ x._4).getOrElse(List.empty), status)
  }

  private def getId(node: Node): String = node.attribute("id").get.toString

  def buildDCR(events: Seq[(String, String)], cs: Seq[(Constraint, (String, String))],
    runtime: Map[String, Map[String, Boolean]]): Option[DCR] = {
    def getOrDefault(id: String, att: String, runtime: Map[String, Map[String, Boolean]]): Boolean =
      runtime.get(id).flatMap(_.get(att)).getOrElse(false)
      Try {
        new DCR {
          override val activities = events.map{x => 
            ActivityImpl(x._1, x._2, 
              included = getOrDefault(x._1, "included", runtime),
              executed = getOrDefault(x._1, "executed", runtime),
              pending = getOrDefault(x._1, "pending", runtime)
              )
          }.toSet
          private val map = activities.map(x => x.id -> x).toMap
          override val constraints = 
            cs.groupBy(_._1).map(x => (x._1 -> 
              x._2.foldLeft[Map[this.Activity, Set[this.Activity]]](Map.empty)
                { case (acc, (_, (act1, act2))) =>
                  val a1 = map.get(act1).get
                  val a2 = map.get(act2).get
                  if (acc contains a1)
                    acc + (a1 -> (acc.get(a1).get + a2))
                  else
                    acc + (a1 -> Set(a2))
                }
              )
            )

          override val reversedConstraints = constraints.keys.map { typ =>
            typ -> ( constraints.get(typ).get.foldLeft[Map[Activity, Set[Activity]]](Map.empty){
              case (acc, (key, value)) =>
                value.foldLeft[Map[Activity, Set[Activity]]](acc){case (inneracc, v) =>
                  if (inneracc contains v)
                    inneracc + (v -> (inneracc.get(v).get + key))
                  else
                    inneracc + (v -> Set(key))
                }
            })
          }.toMap
        }
      }.toOption
  }

  private def extractSourceAndTarget(node: Node): (String, String) = {
    (node.attributes("sourceId").head.toString, node.attribute("targetId").get.head.toString)
  }

  private def activitiesTypeMap(me: DCR, to: DCR): to.ActivitiesType = 
    me.activities.asInstanceOf[to.ActivitiesType]

  private def constraintsTypeMap(from: DCR, to: DCR): to.ConstraintsType = 
    from.constraints.asInstanceOf[to.ConstraintsType]
  private def reversedConstraintsTypeMap(from: DCR, to: DCR): to.ConstraintsType = 
    from.reversedConstraints.asInstanceOf[to.ConstraintsType]
}

trait DCR { 
  type ActivitiesType = Set[Activity]
  type ConstraintsType = Map[DCR.Constraint, Map[Activity, Set[Activity]]]
  trait Activity {
    def id: String
    def name: String
    def included: Boolean
    def excluded: Boolean = !included
    def pending: Boolean
    def executed: Boolean
  }

  def constraints: ConstraintsType

  def reversedConstraints: ConstraintsType

  def activities: Set[Activity]

  implicit def conv(act: Activity): ActivityOps = new ActivityOps(act)
  case class ActivityOps(act: Activity) {
    def includes: Set[Activity] = constraints.get(DCR.Include).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def excludes: Set[Activity] = constraints.get(DCR.Exclude).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def responses: Set[Activity] = constraints.get(DCR.Response).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def conditions: Set[Activity] = constraints.get(DCR.Condition).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def includesThis: Set[Activity] = reversedConstraints.get(DCR.Include).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def excludesThis: Set[Activity] = reversedConstraints.get(DCR.Exclude).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def responsesThis: Set[Activity] = reversedConstraints.get(DCR.Response).flatMap(_.get(act))
      .getOrElse(Set.empty)
    def conditionsThis: Set[Activity] = reversedConstraints.get(DCR.Condition).flatMap(_.get(act))
      .getOrElse(Set.empty)
  }

  def addRelation(typ: DCR.Constraint, source: Activity, target: Activity): DCR = {
    val me = DCR.this
    val blah: this.Activity = source
    val outerConstraints = constraints
    new DCR {
      def activities = DCR.activitiesTypeMap(me, this)
      private val current = outerConstraints.get(typ).get.get(source)
      private val ss = current.map(x => if (x.contains(target)) x else x + target)
      private val converted = DCR.constraintsTypeMap(me, this)
      override val constraints = 
        converted + 
          (typ -> 
            (converted.get(typ).get + 
              (source.asInstanceOf[this.Activity] -> 
                ss.map(_.map(_.asInstanceOf[this.Activity])).getOrElse(Set.empty))))
      private val reversed = DCR.reversedConstraintsTypeMap(me, this)
      private val reversedSet = outerConstraints.get(typ).get.get(target)
        .map(x => if (x.contains(source)) x else x + source)
        .map(_.map(_.asInstanceOf[Activity])).getOrElse(Set.empty)
      override val reversedConstraints = 
        reversed + 
          (typ ->
            (converted.get(typ).get +
              (target.asInstanceOf[Activity] ->
                reversedSet)))
    }
  }

  def copy: DCR = {
    val me = this
    new DCR {
      override val activities = DCR.activitiesTypeMap(me, this)
      override val constraints = DCR.constraintsTypeMap(me, this)
      override val reversedConstraints = DCR.reversedConstraintsTypeMap(me, this)
    }
  }

  override def toString: String = activities.mkString("\n")

  def addActivity(id: String, name: String = "", included: Boolean = false, 
    executed: Boolean = false, pending: Boolean = false): DCR = {
      val me = this
      new DCR {
        override val activities = 
          DCR.activitiesTypeMap(me, this) + 
            new ActivityImpl(id, name, included, executed, pending)
        override val constraints = 
          DCR.constraintsTypeMap(me, this)
        override val reversedConstraints = 
          DCR.reversedConstraintsTypeMap(me, this)
      }
  }

  private[DCR] case class ActivityImpl(id: String, name: String, 
    included: Boolean, executed: Boolean, pending: Boolean) extends Activity

  def toXmlString = {
    <dcrgraph>
      <specification>
        <resources>
          <events>
          {activities.map{(a: Activity) => val id = a.id; <event id={id} />}}
          </events>
          <labels>
          {activities.map{(a: Activity) => val id = a.id; <label id={id} />}}
          </labels>
          <labelMappings>
          {activities.map{(a: Activity) => 
              val id = a.id; <labelMapping eventId={id} labelId={id} />
            }}
          </labelMappings>
        </resources>
        <constraints>
          <conditions>
          {getRelations(constraints.get(DCR.Condition).get){(from, to) =>
              <condition sourceId={from} targetId={to} />
            }}
          </conditions>
          <responses>
          {getRelations(constraints.get(DCR.Response).get){(from, to) =>
              <response sourceId={from} targetId={to} />
            }}
          </responses>
          <excludes>
          {getRelations(constraints.get(DCR.Exclude).get){(from, to) =>
              <exclude sourceId={from} targetId={to} />
            }}
          </excludes>
          <includes>
          {getRelations(constraints.get(DCR.Include).get){(from, to) =>
              <include sourceId={from} targetId={to} />
            }}
          </includes>
        </constraints>
      </specification>
      <runtime>
        <marking>
          <executed>
          {getEventXml(_.executed)}
          </executed>
          <included>
          {getEventXml(_.included)}
          </included>
          <pendingResponses>
          {getEventXml(_.pending)}
          </pendingResponses>
        </marking>
      </runtime>
    </dcrgraph>
  }

  def getRelations(m: Map[Activity, Set[Activity]])(wrap: (String, String) => scala.xml.Elem) = {
    val tos = m.keys.flatMap{ (from: Activity) =>
      val tos = m.get(from).getOrElse(Set.empty)
      tos.map{(to: Activity) => wrap(from.id, to.id)}
    }
  }

  def getEventXml(filter: Activity => Boolean) = {
    activities.filter(filter).map{(a: Activity) => val id = a.id; <event id={id} />}
  }

  override def equals(o: Any): Boolean = o match {
    case other: DCR => 
      (activities zip other.activities).forall{ case (a1, a2) =>
        a1.id == a2.id && a1.name == a2.name && a1.executed == a2.executed &&
        a1.pending == a2.pending && a1.included == a2.included
      } 
    case _ => false
  }
}
