/* 
* The MIT License
*
* Copyright (c) 2015 ECOLE POLYTECHNIQUE FEDERALE DE LAUSANNE, Switzerland, 
* Group Fellay
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the "Software"),
* to deal in the Software without restriction, including without limitation 
* the rights to use, copy, modify, merge, publish, distribute, sublicense, 
* and/or sell copies of the Software, and to permit persons to whom the Software
* is furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
* SOFTWARE.
*/

package mybiotools.tabular

import TabSerializationPrivate._

trait SaddleVecProtocol extends Protocol {

  implicit def vecTableFormat[T](implicit fmt: RowFormat[Tuple1[T]]): TableFormat[Tuple1[Vector[T]]] = new TableFormat[Tuple1[Vector[T]]] {
    def writes(ts: Tuple1[Vector[T]], h: Option[IndexedSeq[String]]) = {
      val headerFromRowFormat = fmt.header(Tuple1(ts._1.apply(0)))
      val header = (h, headerFromRowFormat) match {
        case (None, None) => None
        case (None, Some(x)) => Some(x)
        case (Some(y), Some(x)) if x.size > y.size => {
          Some(x.patch(0, y, y.size))
        }
        case (Some(y), Some(x)) => Some(y)
        case (x, None) => x
      }
      Table(ts._1.toSeq.map(t => toRow(Tuple1(t), h)(fmt)), header)
    }
    def reads(tab: Table) = Tuple1(Vector(tab.rows.map(t => fromRow(t, tab.header)(fmt)._1): _*))
  }

<#list 2..22 as i> 
<#assign typeParams><#list 1..i-1 as j>T${j}<#if i-1 !=j>,</#if></#list></#assign>
    <#assign typeParamsVec><#list 1..i-1 as j>Vector[T${j}]<#if i-1 !=j>,</#if></#list></#assign>

  implicit def vecTableFormatWithSeq${i}[${typeParams},T${i}](implicit fmt: RowFormat[Tuple${i}[${typeParams},Seq[T${i}]]]): TableFormat[Tuple${i}[${typeParamsVec},Seq[Vector[T${i}]]]] = new TableFormat[Tuple${i}[${typeParamsVec},Seq[Vector[T${i}]]]] {
    def writes(ts: Tuple${i}[${typeParamsVec},Seq[Vector[T${i}]]], h: Option[IndexedSeq[String]]) = {
      val headerFromRowFormat = Some(Vector(<#list 1..i-1 as j>"V${j}"<#if i-1 !=j>,
  	</#if></#list>) ++ ts._${i}.toSeq.zipWithIndex.map(x => "V"+(x._2+1+${i-1})))
      val header = (h, headerFromRowFormat) match {
        case (None, Some(x)) => Some(x)
        case (Some(y), Some(x)) if x.size > y.size => {
          Some(x.patch(0, y, y.size))
        }
        case (Some(y), Some(x)) => Some(y)
      }
      val zipped = 
      (
      	 for(k <- 0 until ts._1.length) yield {
  	(<#list 1..i-1 as j>ts._${j}.apply(k)<#if i-1 !=j>,
      		</#if></#list>,ts._${i}.map(_.apply(k)))
  			}
      	)
      Table(
      	zipped.map(t => toRow(t, h)(fmt)),
      	 header)
    }
     def reads(tab: Table) = {
      val t = tab.rows.map(t => fromRow(t, tab.header)(fmt))

      val x = for(k <- 0 until t.head._${i}.length) yield {
      	Vector(t.map(_._${i}(k)):_*)
      }

      Tuple${i}(
      	<#list 1..i-1 as j>Vector(t.map(_._${j}): _*)<#if i-1 !=j>,
      		</#if></#list>,x
      	)
    } 
  }

    </#list>


    <#list 2..22 as i> 
    <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
    <#assign typeParamsVec><#list 1..i as j>Vector[T${j}]<#if i !=j>,</#if></#list></#assign>

  implicit def vecTableFormat${i}[${typeParams}]
  	(implicit fmt: RowFormat[Tuple${i}[${typeParams}]]): TableFormat[Tuple${i}[${typeParamsVec}]] = 
  		new TableFormat[Tuple${i}[${typeParamsVec}]] {
    def writes(ts: Tuple${i}[${typeParamsVec}], h: Option[IndexedSeq[String]]) = {
      val headerFromRowFormat = fmt.header(
      	Tuple${i}(
      		<#list 1..i as j>ts._${j}.apply(0)<#if i !=j>,
      		</#if></#list>
      		)
      	)
      val header = (h, headerFromRowFormat) match {
        case (None, None) => None
        case (None, Some(x)) => Some(x)
        case (Some(y), Some(x)) if x.size > y.size => {
          Some(x.patch(0, y, y.size))
        }
        case (Some(y), Some(x)) => Some(y)
        case (x, None) => x
      }
      val zipped = (
      	 for(k <- 0 until ts._1.length) yield {
  	(<#list 1..i as j>ts._${j}.apply(k)<#if i !=j>,
      		</#if></#list>)
  			}
      	)
      Table(
        zipped.toSeq.map(t => toRow(t, h)(fmt)),
        header)
    }
    def reads(tab: Table) = {
      val t = tab.rows.map(t => fromRow(t, tab.header)(fmt))

      Tuple${i}(
      	<#list 1..i as j>Vector(t.map(_._${j}): _*)<#if i !=j>,
      		</#if></#list>
      	)
    }
  }

  </#list>

}