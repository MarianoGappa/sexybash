package sexybash

import java.io.{PrintWriter, File}
import org.scalatest.{BeforeAndAfterAll, Matchers, FunSpec}

class MainTest extends FunSpec with Matchers with BeforeAndAfterAll {
  val testScriptFilename = "script.sb"

  describe("SexyBash") {
    it("should display help when used incorrectly") {
      Main.eval(Array()) should be(Main.Help)
      Main.eval(Array("file1", "file2")) should be(Main.Help)
    }

    it("should parse an empty file") {
      createFileWithContent("")
      Main.eval(Array(testScriptFilename)) should be("")
    }

    it("should parse a file with no StringBlocks as empty") {
      createFileWithContent(" Plain text file with no parseable content ")
      Main.eval(Array(testScriptFilename)) should be("")
    }

    it("should parse a file with a StringBlock") {
      val stringBlock =
        """[
          |   some
          |   escaped
          |   content
          |   even
          |     with
          |     indentation
          |   in it
          |]
        """.stripMargin

      createFileWithContent(stringBlock)

      Main.eval(Array(testScriptFilename)) should be
       """some
         |escaped
         |content
         |even
         |  with
         |  indentation
         |in it""".stripMargin
    }


  }

  override def afterAll() = deleteTestFile()

  def createFileWithContent(content: String) = {
    val writer = new PrintWriter(new File(testScriptFilename))
    writer.write(content)
    writer.close()
  }

  def deleteTestFile() = {
    new File("script.sb").delete()
  }

}
