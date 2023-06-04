/*
 * Copyright (c) 2023 Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

package ReportsBuilder

import ReportsBuilder.TableStructure.{Column, Header}
import Utilz.CreateLogger
import org.slf4j.Logger

enum TableStructure:
  case Column(header: String)
  case Row(cells: String*)
  case ColumnSpan(header: Column, span: Int)
  case RowSpan(cell: String, span: Int, rows: Row*)
  case Header(columns: Column*)
  case Table(header: Header, rows: (Row|RowSpan)*)

  def toHTML(tableName: String): String =
      s"""
        |<html>
        |<head>
        |  <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
        |  <title>$tableName</title>
        |</head>
        |<body>
        |""".stripMargin
      + innerHTML
      + "</body>\n</html>"
  end toHTML
  def innerHTML: String = this match
    case Table(header, rows*) => s"<table border = \"1\">\n${header.innerHTML}\n${rows.map(_.innerHTML).mkString}\n</table>\n"
    case Header(columns*) => s"\n<tr>${columns.map(_.innerHTML).mkString}</tr>"
    case Column(header: String) => s"\n<th>$header</th>"
    case Row(cells*) => s"\n<tr>${cells.map(cell => s"\n<td>$cell</td>").mkString}</tr>".mkString
    case ColumnSpan(header, span) => s"""<th colspan=$span>$header</th>"""
    case RowSpan(cell, span, rows*) => s"<td rowspan=$span>$cell</td>${rows.map(_.toHTML).mkString}"
    
object ConstructTable:
  val logger: Logger = CreateLogger(classOf[TableStructure])
  @main def runTable(args: String*): Unit =
    import ReportsBuilder.TableStructure.{Column, Row}
    logger.info("File /Users/drmark/Library/CloudStorage/OneDrive-UniversityofIllinoisChicago/Github/SeaPhish/GenericSimUtilities/src/main/scala/ReportsBuilder/Table.scala created at time 4:57 PM")
    val table = TableStructure.Table(Header(Column("Col1"),Column("Col2")), Row("Cell1", "Cell2"), Row("Cell3", "Cell4"))
    println(table.toHTML("Table XX"))
