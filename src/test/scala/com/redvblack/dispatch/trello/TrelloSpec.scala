package com.redvblack.dispatch.trello

import org.specs2.mutable._
import org.specs2.specification._

class TrelloSpec extends Specification {
  "Trello " should {
      "Load config from home directory" in {
        import com.redvblack.dispatch.trello._
        // temporarily reset the home directory
        val originalHome = System.getProperty("user.home")
        System.setProperty("user.home", System.getProperty("java.io.tmpdir"))
        // double check that there isn't a config file there
        import java.io.File
        new File(System.getProperty("user.home") + "/.trello").exists must_== false
        // attempt to load the props
        val shouldBeMissing = Trello.loadConfigFromHomeDirectory
        // confirm that it didn't
        shouldBeMissing._1.startsWith("Token missing") must_== true
        shouldBeMissing._2.startsWith("Key missing") must_== true
        // put a config file there
        val config = "token=ATestToken\nkey=ATestKey"
        val out = new java.io.FileWriter(System.getProperty("user.home") + "/.trello" )
        out.write(config)
        out.close
        // attempt to load the props
        val shouldBeThere = Trello.loadConfigFromHomeDirectory
        // confirm that it did
        shouldBeThere._1 must_== "ATestToken"
        shouldBeThere._2 must_== "ATestKey"
        // now delete it
        new File(System.getProperty("user.home") + "/.trello").delete must_== true
        System.setProperty("user.home", originalHome)
        System.getProperty("user.home") must_== originalHome
      }
  }
}
