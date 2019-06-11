package compile
import java.io._

import util.CLI

import scala.Console

// Begin parser/scanner imports
import antlr.CommonAST
import antlr.collections.AST
import edu.mit.compilers.grammar.{DecafParser, DecafScanner, DecafScannerTokenTypes}

object Compiler {
  val tokenMap = Map(
    DecafScannerTokenTypes.ID -> "IDENTIFIER",
    DecafScannerTokenTypes.CHAR_LITERAL -> "CHARLITERAL",
    DecafScannerTokenTypes.STRING_LITERAL -> "STRINGLITERAL",
    DecafScannerTokenTypes.INT_LITERAL -> "INTLITERAL",
    DecafScannerTokenTypes.TK_true -> "BOOLEANLITERAL",
    DecafScannerTokenTypes.TK_false -> "BOOLEANLITERAL")

  var outFile = if (CLI.outfile == null) Console.out else new java.io.PrintStream(
    new java.io.FileOutputStream(CLI.outfile))

  def main(args: Array[String]): Unit = {
    CLI.parse(args, Array[String]());
    if (CLI.target == CLI.Action.SCAN) {
      scan(CLI.infile)
      System.exit(0)
    } else if (CLI.target == CLI.Action.PARSE) {
      if(parse(CLI.infile) == null) {
        System.exit(1)
      }
      System.exit(0)
    }
  }

  def scan(fileName: String) {
    try {
      val inputStream: FileInputStream = new java.io.FileInputStream(fileName)
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      scanner.setTrace(CLI.debug)
      while (true) {
        try {
          val head = scanner.nextToken()
          if (head.getType == DecafScannerTokenTypes.EOF) {
            return
          } else {
            val tokenType = tokenMap.getOrElse(head.getType, "")
            outFile.println(head.getLine + (if (tokenType ==  "") "" else " ") + tokenType + " " + head.getText)
          }
        } catch {
          case ex: Exception => {
            Console.err.println(CLI.infile + " " + ex)
            scanner.consume()
          }
        }
      }
    } catch {
      case ex: Exception => Console.err.println(ex)
    }
  }

  def parse(fileName: String): CommonAST = {
    /**
      * Parse the file specified by the filename. Eventually, this method
      * may return a type specific to your compiler.
      */
    var inputStream : java.io.FileInputStream = null
    try {
      inputStream = new java.io.FileInputStream(fileName)
    } catch {
      case _: FileNotFoundException => Console.err.println("File " + fileName + " does not exist"); return null
    }
    try {
      val scanner = new DecafScanner(new DataInputStream(inputStream))
      val parser = new DecafParser(scanner)

      parser.setTrace(CLI.debug)
      parser.program()
      val t = parser.getAST.asInstanceOf[CommonAST]
      if (parser.getError) {
        println("[ERROR] Parse failed:")
        return null
      } else if (CLI.debug){
        traverseParseTree(t, "")
      }
      t
    } catch {
      case e: Exception => Console.err.println(CLI.infile + " " + e)
        null
    }
  }

  def traverseParseTree(tree: AST, indentation: String) {
    println(indentation + "Node: " + tree.getText + " " + tree.getType)
    if (tree.getFirstChild != null) {
      traverseParseTree(tree.getFirstChild, indentation + "  ")
    }
    if (tree.getNextSibling != null) {
      traverseParseTree(tree.getNextSibling, indentation)
    }
  }
}
