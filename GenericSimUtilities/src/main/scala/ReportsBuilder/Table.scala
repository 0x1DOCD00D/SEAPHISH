/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package ReportsBuilder

import ReportsBuilder.SPAN.{COLUMNSPAN, ROWSPAN}
import ReportsBuilder.TableStructure.Header
import Utilz.CreateLogger
import org.slf4j.Logger

enum SPAN:
  case COLUMNSPAN, ROWSPAN
end SPAN

enum TableStructure:
  case Row(cells: Cell*)
  case Cell(data: String, span: Option[(SPAN, Int)] = None)
  case Header(cells: Cell*)
  case Table(caption: String, headers: List[Header], rows: List[Row])

  def toHTML(tableName: String): String =
      s"""
        |<html>
        |<head>
        |  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        |  <title>$tableName</title>
        |</head>
        |<body>
        |""".stripMargin
      + innerHTML().mkString
      + "</body>\n</html>"
  end toHTML

  def decodeSpan(span: Option[(SPAN, Int)]): String = span match
    case Some((SPAN.COLUMNSPAN, n)) => s"colspan=\"$n\""
    case Some((SPAN.ROWSPAN, n)) => s"rowspan=\"$n\""
    case None => ""
  end decodeSpan

  def innerHTML(header: Boolean = false): String = this match
    case Table(caption, headers, rows) => s"<table border = \"1\">\n<caption>$caption</caption>\n${headers.map(_.innerHTML().mkString).mkString}\n${rows.map(_.innerHTML().mkString).mkString}\n</table>\n".mkString
    case Header(cells*) => s"\n<tr>${cells.map(_.innerHTML(true)).mkString}</tr>".mkString
    case Row(cells*) => s"\n<tr>${cells.map(cell => cell.innerHTML()).mkString}</tr>"
    case Cell(data, span: Option[(SPAN, Int)]) =>
      val str = decodeSpan(span).trim
      if str.nonEmpty then s"<th $str>$data</th>" else if header then s"<th>$data</th>" else s"<td>$data</td>"
  end innerHTML

object ConstructTable:
  val logger: Logger = CreateLogger(classOf[TableStructure])

  def apply(tableID: Int, caption: String, headers: List[Header]): TableStructure.Table = TableStructure.Table(caption, headers, List.empty)
  def startNewRow(tableID: Int) = ???
  def addCell(tableID: Int, cell: TableStructure.Cell) = ???

  @main def runTable(args: String*): Unit =
    import ReportsBuilder.TableStructure.*
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GenericSimUtilities/src/main/scala/ReportsBuilder/Table.scala created at time 4:57 PM")
    val table = TableStructure.Table("Life Expectancy By Current Age",
      List(
        Header(Cell("High", Some((COLUMNSPAN,4))), Cell("Low", Some((COLUMNSPAN,4)))),
        Header(Cell("65", Some((COLUMNSPAN,2))), Cell("40", Some((COLUMNSPAN,2))), Cell("20", Some((COLUMNSPAN,2))), Cell("10", Some((COLUMNSPAN,2)))),
        Header(Cell("Men"), Cell("Women"),Cell("Men"),Cell("Women"), Cell("Men"), Cell("Women"), Cell("Men"), Cell("Women"))
      ),
      List(
      Row(Cell("81"), Cell("79"),Cell("83"),Cell("85"), Cell("90"), Cell("101"), Cell("90"), Cell("101")),
      Row(Cell("71"), Cell("89"),Cell("81"),Cell("78"), Cell("91"), Cell("103"), Cell("90"), Cell("101")))
    )
    println(table.toHTML("Table XX"))
