
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
  val config = loadConfigFromHomeDirectory
  val token = config._1
  val key = config._2
  val params:Map[String, String] = Map(("key" -> key), ("token" -> token), ("cards" -> "all"), ("card_fields" -> "all"), ("lists" -> "all"), ("checklists" -> "all"))
  val packageName = "com.redvblack.dispatch.trello"
  val projectRoot = "/Users/bufferine/Dev/TrelloSpecs2Generator"
  val testPath = "/src/test/scala"
  val specName = "TrelloSpec"
  val specFileName = projectRoot + testPath + "/" + packageName.replace(".", "/") + "/GeneratedSpec.scala"
  val boardId = "BoardId"

  def generateOrUpdate = {
      if (!(new java.io.File(specFileName).exists)) {
        generateTestCases
      }else {
        val specs = loadExistingSpecs
        val board = loadBoard
        val compare = compareSpecToBoard(specs, board)
        insertIntoSpec(specs, board, compare)
      }
  }

  def loadConfigFromHomeDirectory:Pair[String, String] = {
    val homeDir = System.getProperty("user.home")
    try {
      val trelloConf:List[String] = scala.io.Source.fromFile(homeDir + "/.trello").getLines.toList
      val token = trelloConf.filter(_.startsWith("token")).headOption.map(t => t.substring(t.indexOf("=") + 1).trim).getOrElse("Token missing from " + homeDir + "/.trello")
      val key = trelloConf.filter(_.startsWith("key")).headOption.map(t => t.substring(t.indexOf("=") + 1).trim).getOrElse("Key missing from " + homeDir + "/.trello")
      Pair(token, key)
    }catch {
      case x => {
        x.printStackTrace
        Pair("Token missing - Error Loading Token " + x.getMessage, "Key missing - Error Loading Key " + x.getMessage)
      }
    }
  }

  def loadExistingSpecs:Map[String, Fragment] = {
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
    val tabDepth = 1
    List("\n" + ("\t" * tabDepth) + "\"Card[" + card.id + "](" + card.url.get +") " + card.name.get + "\" should {") :::
      // for each checklist generate a sub test
      (card.idChecklists.map(checkListId => {
        toSpec(Http(Checks(checkListId).checkList).get, tabDepth + 1)
      }).foldLeft(List[String]())((acc, next) => next ::: acc)) :::
      List("\n " + ("\t" * (tabDepth + 1)) + "pending" + "\n" + ("\t" * tabDepth) + "}")
  }
  def toSpec(checkItem:CheckItem, tabDepth:Int):List[String] = {
    List("\n" + ("\t" * tabDepth) + "\"CheckItem[" + checkItem.id + "] " + checkItem.name.get + "\" in {",
    "\n" + ("\t" * (tabDepth + 1)) + "pending",
    "\n" + ("\t" * tabDepth) + "}")
  }
  def toSpec(checkList:CheckList, tabDepth:Int):List[String] =  {
    List("\n" + ("\t" * tabDepth) + "\"CheckList[" + checkList.id + "] " + checkList.name.get + "\" should {") :::
      checkList.checkItems.flatMap(item => {
        toSpec(item, tabDepth + 1)
      }) :::
    List("\n" + ("\t" * (tabDepth + 1)) + "pending",
    "\n" + ("\t" * tabDepth) + "}"
        )
  }

  def insertIntoSpec(specs:Map[String, Fragment], trello:BoardCase, missings:Pair[List[String], List[String]]) = {
    var specFile:List[String] = try {
      scala.io.Source.fromFile(specFileName).getLines.toList
    }catch {
      case x => List[String]()
    }

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
    val testCase = List(" package " + packageName +
    " \nimport org.specs2.mutable._ " +
    " \nimport org.specs2.specification._ " +
    " \nclass GeneratedSpec extends Specification { ") :::
    board.cards.map(card => {
      toSpec(card)
    }).foldLeft(List[String]())((acc, next) => acc ::: next) ::: List("\npending\n}")

    val out = new java.io.FileWriter(specFileName)
    out.write(testCase.mkString(""))
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
