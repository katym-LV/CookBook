import java.io.FileWriter
import java.nio.charset.CodingErrorAction

import scala.io.Codec
import scala.util.Success

object CookBook extends App {
  val srcName = "https://www.gutenberg.org/files/13177/13177-8.txt"
  val destName = "13177-8-results.txt"

  def getFileLines(srcName: String): Seq[String] = {
    implicit val codec:Codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    val filePointer = scala.io.Source.fromURL(srcName)
    val lines = filePointer.getLines.toSeq
    filePointer.close()
    lines
  }

  def extractRecipes(fileLines: Seq[String]): Seq[String] = {
    val establishedString = "ESTABLISHED 1780"
    val isHeadline = (l: String) => """^[A-Z]+\b.*$""".r.matches(l)
    val isIngredient = (l: String) => """^ {4,8}[^\s*"].+$""".r.matches(l)
    val isBadHeadline = (l: String) => (""".*(""" + establishedString + """|WALTER BAKER &).*""").r.matches(l)
    val isEndOfBook = (l: String) => l == establishedString + "."

    val filteredLines = fileLines.filter(l => isHeadline(l) || isIngredient(l))

    var recipes: Seq[String] = Seq()

    var hadIngredient = false
    var recipe = ""
    for (line <- filteredLines) {
      if (isHeadline(line) && !isBadHeadline(line)) {
        if (hadIngredient) {
          recipes = recipes :+ recipe + "\n"
          hadIngredient = false
        }
        recipe = line + "\n"
      } else if (isIngredient(line)) {
        recipe += "\n" + line
        hadIngredient = true
      } else if (isEndOfBook(line)) {
        return recipes
      }
    }
    recipes
  }

  def saveRecipes(destName: String, recipes: Seq[String]): Success.type = {
    println(s"Saving my sequence to file $destName")
    val fw = new FileWriter(destName)
    fw.write(recipes.mkString("\n"))
    fw.close()
    scala.util.Success
  }

  val cookBookLines = getFileLines(srcName)
  val allRecipes = extractRecipes(cookBookLines)
  saveRecipes(destName, allRecipes)
}
