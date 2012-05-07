
package com.redvblack.dispatch.trello
import dispatch._
import json._
import JsHttp._
import oauth._
import oauth.OAuth._
import dispatch.Request._
import sys.process._
import org.specs2.specification._

object Trello {
  val host = :/("api.trello.com") / "1"
//  val search = :/("search.twitter.com")
  val token =  "YourTokenHere"
  val key = "YourKeyHere"
  val params:Map[String, String] = Map(("key" -> key), ("token" -> token), ("cards" -> "all"), ("card_fields" -> "all"), ("lists" -> "all"), ("checklists" -> "all"))
  val packageName = "com.redvblack.trello"
  val projectRoot = "Root of project to create test in"
  val testPath = "/src/test/scala"
  val specName = "TrelloSpec"
  val specFileName = projectRoot + testPath + "/" + packageName.replace(".", "/") + "generatedspec.scala"
  val boardId = "boardId"

  def loadExistingSpecs:Map[String, Fragment] = {
    /*
    Text([4fa3ef06f5d35c5e3a021843] Event Create should)
    Text([4fa3f15a9b4908fb570b2e69] Checklist should)
    Example([4fa3f16c9b4908fb570b3dc1] Admin interface for event creation)
    Example([4fa3f17d9b4908fb570b4c7a] Model supporting event creation and tagging)
     */
    val es = Class.forName(packageName + "." + specName)

    val instance = es.newInstance
    val field = es.getDeclaredField("specFragments")
      field.setAccessible(true)
    val fragments:org.specs2.specification.Fragments = field.get(instance).asInstanceOf[Fragments]
    // anything with a string inside square brackets, assume to be generated from Trello

    fragments.fragments.flatMap(f => {
      val IDed = ".*\\[(.*)\\](.*)".r
      f.toString match {
        case IDed(id, text) =>  Some(id -> f)
        case _ => None
      }
    }).toMap
  }


  def loadBoard:BoardCase = {
    // grab the boards, and the cards for the boards
    Http(Board(boardId).get).get
  }

  def compareSpecToBoard(specs:Map[String, Fragment], trello:BoardCase):Pair[List[String], List[String]] = {
    // flatten out all the ids available in the board
    val cardMap = trello.cards.map(c => c.id -> c).toMap
    val listMap = trello.lists.map(l => l.id -> l).toMap
    val checkListsMap = trello.checklists.map(c => c.id -> c).toMap
    val checkListItems = checkListsMap.values.flatMap(cl => {
      cl.checkItems.map(cli => cli.id -> cli)
    }).toMap
    // id in the fragment that doesn't exist in the board's board, list, checklist, checklistitem put it in a deleted from trello list
    val missingFromTrello = specs.keys.filter(k => {
      cardMap.get(k) == None && listMap.get(k) == None && checkListsMap.get(k) == None && checkListItems.get(k) == None
    })
    val missingFromSpecs = (cardMap.keys.toList ::: listMap.keys.toList :::  checkListsMap.keys.toList ::: checkListItems.keys.toList).filter(k => specs.get(k) == None)
    Pair(missingFromTrello.toList, missingFromSpecs.toList)
  }

  def toSpec(card:Card):List[String] = {
    List("\"[" + card.id + "] " + card.name.get + "\" should {") :::
      // for each checklist generate a sub test
      (card.idChecklists.map(checkListId => {
        val checkList = Http(Checks(checkListId).checkList).get
        List("\n\t\"[" + checkList.id + "] " + checkList.name.get + "\" should {") :::
          (checkList.checkItems.map(item => {
            "\n\t\t\"[" + item.id + "] " + item.name.get + "\" in {\n\t\t\tpending\n\t\t}"
          }).foldLeft(List[String]())((acc, next) => next :: acc)) :::  List("\n\t\tpending\n\t}")
      }).foldLeft(List[String]())((acc, next) => next ::: acc)) :::
      List("\npending\n}")
  }

  def insertIntoSpec(specs:Map[String, Fragment], trello:BoardCase, missings:Pair[List[String], List[String]]) = {
    var specFile:List[String] = scala.io.Source.fromFile(specFileName).getLines.toList

    // missing from specs
    missings._2.foreach(m => {
      if (m == trello.id) // missing board - ie. entire missing specs class - can just append to the end
      if (trello.lists.map(_.id).contains(m)) {// missing list {
        // needs to be put in one bracket from the end
      }
      if (trello.cards.map(_.id).contains(m)) {// missing card
        // find the list in the specFile that this should go on
        trello.cards.filter(_.id == m).map(card => {
          card.idList.map(listId => {
            specFile.filter(_.contains(listId)).headOption.map(listRow => {
              if (specFile.indexOf(listRow) > 0) {
                val split = specFile.splitAt(specFile.indexOf(listRow))
                specFile = split._1 ::: toSpec(card) ::: split._2
              }else{
                // add it to the last row -1 (NB - Assumes that's where the bracket is)
                val split = specFile.splitAt(specFile.size -1)
                specFile = split._1 ::: toSpec(card) ::: split._2
              }
            }).getOrElse({
              val split = specFile.splitAt(specFile.size -1)
              specFile = split._1 ::: toSpec(card) ::: split._2
            })
          })
        })

        // add on the line straight after
      }
      if (trello.checklists.map(_.id).contains(m)) {// missing checklist
        // find the card in the specFile that this should go on
        // add it straight after
      }
//      if (trello.checklistItems.map(_.id).contains(m)) { // missing checklistItem
        // find the checkList this belongs in, add it straight after
//      }
    })
//    println(specFile.mkString("\n"))
    val out = new java.io.FileWriter(specFileName)
    out.write(specFile.mkString("\n"))
    out.close
  }



  def generateTestCases = {
    // grab the boards, and the cards for the boards
    val board:BoardCase = Http(Board(boardId).get).get
    // for each card, generate a test
    val testCase = " package " + packageName +
    " \nimport org.specs2.mutable._ " +
    " \nimport org.specs2.specification._ " +
    " \nclass RiakClientSpec extends Specification { " +
    board.cards.map(card => {
      "\n\"[" + card.id + "] " + card.name.get + "\" should {" +
      // for each checklist generate a sub test
         card.idChecklists.map(checkListId => {
           val checkList = Http(Checks(checkListId).checkList).get
           "\n\t\"[" + checkList.id + "] " + checkList.name.get + "\" should {" +
           checkList.checkItems.map(item => {
             "\n\t\t\"[" + item.id + "] " + item.name.get + "\" in {\n\t\t\tpending\n\t\t}"
           }).foldLeft("\n")((acc, next) => acc + next) + "\n\t\tpending\n\t}"
         }).foldLeft("\n")((acc, next) => acc + next) +
      "\npending\n}"
    }).foldLeft("\n")((acc, next) => acc + "\n" + next) + "\npending\n}"

    val out = new java.io.FileWriter(specFileName)
    out.write(testCase)
    out.close
  }
}
object Checks extends Request(Trello.host / "checklists") {
}
case class Checks(checkListId: String) extends Request(Checks / checkListId) with Js {
  def checkList = this.secure <<? Trello.params ># {
    json => CheckList(json)
  }
}
object CheckList{
  def apply(json:JsValue):Option[CheckList] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    Some(net.liftweb.json.Serialization.read[CheckList](json.toString))
  }
}
object Board extends Request(Trello.host / "boards") {
}
trait BoardField {def value:String}
object Name extends BoardField {val value = "name"}
object Desc extends BoardField {val value = "desc"}
object Closed extends BoardField {val value = "closed"}
object IdOrganization extends BoardField {val value = "idOrganization"}
object Invited extends BoardField {val value = "invited"}
object Pinned extends BoardField {val value = "pinned"}
object Url extends BoardField {val value = "url"}
object Prefs extends BoardField {val value = "prefs"}
object Invitations extends BoardField {val value = "invitations"}
object Memberships extends BoardField {val value = "memberships"}
object LabelNames extends BoardField {val value = "labelNames"}

case class Board(board_id: String) extends Request(Board / board_id) with Js {
  def get = this.secure <<? Trello.params ># {
    json => BoardCase(json)
  }
//  def field(fieldParam:BoardField) = this.secure <<? Trello.params + ("field" -> fieldParam.value)  ># {
//    json => fieldParam(json)
//  }
}
case class CheckList(val id:String, val name:Option[String], val idBoard:Option[String], val checkItems:List[CheckItem])
case class CheckItem(val id:String, val name:Option[String], val checkListType:Option[String], val pos:Option[Int])
case class Prefs(val selfJoin:Boolean, val permissionLevel:String, val voting:String, val invitations:String, val comments:String)
case class Card(val id:String
                , val attachments:List[Attachment]
                , val badges:Option[Badges]
                , val checkItemStates:List[CheckItemState]
                , val closed:Option[Boolean]
                , val desc:Option[String]
                , val due:Option[String]
                , val idBoard:Option[String]
                , val idChecklists:List[String]
                , val idList:Option[String]
                , val idMembers:List[String]
                , val idShort:Option[Int]
                , val labels:List[Label]
                , val name:Option[String]
                , val pos:Option[Int]
                , val url:Option[String])
case class Attachment(val idMember:Option[String]
                      , val name:Option[String]
                      , val url:Option[String]
                      , val date:Option[String]
                      , val bytes:Option[Int]
                      , val _id:Option[String])
case class Badges(val votes:Option[Int]
                  , val viewingMemberVoted:Option[Boolean]
                  , val fogbugz:Option[String]
                  , val checkItems:Option[Int]
                  , val checkItemsChecked:Option[Int]
                  , val comments:Option[Int]
                  , val attachments:Option[Int]
                  , val description:Option[Boolean]
                  , val due:Option[String])
case class CheckItemState(val idCheckItem:String, val state:String)
case class Label(val color:String, val name:String)

case class BoardCase(val name:String
                     , val desc:String
                     , val closed:Boolean
                     , val idOrganization:String
                     , val pinned:Boolean
                     , val url:String
                     , val prefs:Prefs
                     , val id:String
                     , val cards:List[Card]
                     , val lists:List[BoardList]
                     , val members:List[Member]
                     , val checklists:List[CheckList]
                      )
case class Member(val id:String, val avatarSource:Option[String], val bio:Option[String], val fullName:Option[String], val gravatarHash:Option[String]
, idBoards:List[String], val idBoardsInvited:List[String], val idBoardsPinned:List[String], val idOrganizations:List[String], val idOrganizationsInvited:List[String]
, initials:Option[String], val prefs:Option[MemberPrefs], val status:Option[String], val url:Option[String], val username:Option[String])
case class MemberPrefs(val sendSummaries:Boolean, val minutesBetweenSummaries:Int)
case class BoardList(val id:String, val name:Option[String], val closed:Option[Boolean], val idBoard:Option[String], val pos:Option[Int])

object BoardCase {
  def apply(json:JsValue):Option[BoardCase] = {
    implicit val formats = net.liftweb.json.DefaultFormats
    Some(net.liftweb.json.Serialization.read[BoardCase](json.toString))
  }
}


/*
object Search extends Js {
  def apply(query: String, params: (String, String)*) = new SearchBuilder(Map(params: _*), Map.empty[String, String]).q(query)
  class SearchBuilder(params: Map[String, String], headers: Map[String, String]) extends Builder[Handler[List[JsObject]]] {
    private def param(key: String)(value: Any) = new SearchBuilder(params + (key -> value.toString), headers)
    private def header(key: String)(value: String) = new SearchBuilder(params, headers + (key -> value))
    val q = param("q")_
    val lang = param("lang")_
    val rpp = param("rpp")_
    val page = param("page")_
    /** search rate limit conditions are higher for unique userAgents */
    val user_agent = header("User-Agent")_
    val since_id = param("since_id")_
    private def geocode0(unit: String)(lat: Double, lon: Double, radius: Double) =
      param("geocode")(List(lat, lon, radius).mkString(",") + unit)
    val geocode = geocode0("km")_
    val geocode_mi = geocode0("mi")_
    def product = Twitter.search / "search.json" <<? params <:< headers ># ('results ! (list ! obj))
  }

  val to_user_id = 'to_user_id ? num
  val from_user_id = 'from_user_id ? num
  val source = 'source ? str
  val id = 'id ? num
  val text = 'text ? str
  val created_at = 'created_at ? str
  val iso_language_code = 'iso_language_code ? str
  val from_user = 'from_user ? str
}
object Status extends Request(Twitter.host / "statuses") {
  private def public_timeline = this / "public_timeline.json" ># (list ! obj)

  def friends_timeline(consumer: Consumer, token: Token, params: (String, String)*) =
    new FriendsTimelineBuilder(consumer, token, Map(params: _*))

  class FriendsTimelineBuilder(consumer: Consumer, token: Token, params: Map[String, String])
      extends Builder[Handler[List[JsObject]]] {

    private def param(key: String)(value: Any) =
      new FriendsTimelineBuilder(consumer, token, params + (key -> value.toString))
    val since_id = param("since_id")_
    val max_id = param("max_id")_
    val count = param("count")_
    val page = param("page")_
    def product =  Status / "friends_timeline.json" <<? params <@ (consumer, token) ># (list ! obj)
  }

  def update(status: String, consumer: Consumer, token: Token) =
    this / "update.json" << Map("status" -> status) <@ (consumer, token)

  val text = 'text ? str
  val id = 'id ? num
  val user = new Obj('user) with UserProps // Obj assigns context to itself

  def rebracket(status: String) = status replace ("&gt;", ">") replace ("&lt;", "<")
}

case class Status(user_id: String) extends
    Request(Status / "user_timeline" / (user_id + ".json")) with Js {

  def timeline = this ># (list ! obj)
}

trait UserProps {
  // undefined local context for `?` to allow Obj / Js to assign themselves
  implicit val ctx: Option[Obj]
  val followers_count = 'followers_count ? num
  val screen_name = 'screen_name ? str
}

object Users extends 
    Request(Twitter.host / "users") with Js with UserProps {

  def show(screenName: String) =
    this / "show" / (screenName + ".json") ># obj

  def lookup_by_id(ids: List[BigDecimal], includeEntities: Boolean = false) =
    this / "lookup.json" <<? Map("user_id" -> ids.mkString(","), "include_entities" -> includeEntities.toString) ># (list ! obj)

  def lookup_by_id_as(ids: List[BigDecimal], consumer: Consumer, token: Token, includeEntities: Boolean = false) =
    this / "lookup.json" <<? Map("user_id" -> ids.mkString(","), "include_entities" -> includeEntities.toString) <@ (consumer, token) ># (list ! obj)

  val statuses_count = 'statuses_count ? num
  val friends_count = 'friends_count ? num
  val created_at = 'created_at ? str
  val default_profile_image = 'default_profile_image ? bool
  val lang = 'lang ? str
}

object Followers extends
    Request(Twitter.host / "followers" / "ids.json") with Js {

  def get(user: String, cursor: BigDecimal = -1) =
    this <<? Map("screen_name" -> user, "cursor" -> cursor.toString)

  def get_as(user: String, consumer: Consumer, token: Token, cursor: BigDecimal = -1) =
    this <<? Map("screen_name" -> user, "cursor" -> cursor.toString) <@ (consumer, token)

  val previousCursor = 'previous_cursor ? num
  val nextCursor = 'next_cursor ? num
  val ids = 'ids ? list
}

object Account extends
   Request(Twitter.host / "account") with Js {
   /** ip based rate limit status */
   def rate_limit_status = this / "rate_limit_status.json" ># obj
   /** authenticated user rate limit status */
   def rate_limit_status_as(consumer: Consumer, token: Token) =
     this / "rate_limit_status.json" <@(consumer, token) ># obj
   /** authenticated user verify credentials **/
   def verify_credentials_as(consumer: Consumer, accessToken: Token) =
     (this / "verify_credentials.json").secure <@(consumer, accessToken) ># obj
}

object RateLimitStatus extends Js {
  val remaining_hits = 'remaining_hits ? num
  val reset_time_in_seconds = 'reset_time_in_seconds ? num
  val hourly_limit = 'hourly_limit ? num
  val reset_time = 'reset_time ? str
}

*/

