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

// one sample for i=3 that will be generated is:
trait ProductProtocol extends Protocol { 

//   implicit def tuple2EmbeddedFormat[T1,T2](implicit 
//     fmt1: TupleFormat[T1],fmt2: CellFormat[T2]      
//     ): RowFormat[   Tuple2[T1,Seq[T2] ]
// ] = new RowFormat[   Tuple2[T1,Seq[T2] ]
// ]{
//       def reads (tab:Row,header:Option[IndexedSeq[String]]) = {
//         (
//       fromRow[T1](Row(tab.cells.take(fmt1.width))),tab.cells.drop(fmt1.width).map(c => fromCell[T2](c))
//         )
//       }
//       def writes(tuple:    Tuple2[T1,Seq[T2] ],header:Option[IndexedSeq[String]]) = tuple match {
//         case (
//                 t1,t2      
//           ) => Row(IndexedSeq(
//         toCell(t1)
//           ) ++ t2.map(x => toCell(x) )      )
//         case _ => throw new RuntimeException("Tuple" + 2 + " expected")
//       }
//       def header(tuple:Tuple2[T1,Seq[T2] ]) = Some(IndexedSeq(
//               "V1"      
//         ) ++ tuple._2.zipWithIndex.map(p => "V"+(p._2+1).toString ).toIndexedSeq )
//   }

implicit def tuple1Format[T1](implicit 
    fmt1: CellFormat[T1]
    ): RowFormat[   Tuple1[T1]
] = new RowFormat[   Tuple1[T1 ]
]{
      def reads (tab:Row,header:Option[IndexedSeq[String]]) = {
        Tuple1(
      fromCell[T1](tab.cells(1-1))(fmt1)
        )
      }
      def writes(tuple:    Tuple1[T1],header:Option[IndexedSeq[String]]) = tuple match {
        case Tuple1(
                t1     
          ) => Row(IndexedSeq(
        toCell[T1](t1)      
          )  )
          
        case _ => throw new RuntimeException("Tuple" + 1 + " expected")
      }
      def header(tuple:    Tuple1[T1 ]) = Some(IndexedSeq(
              "V1"      
        )
      )
  }

//     implicit def tuple1Format[T1](implicit 
//     fmt1: CellFormat[T1]
//     ): RowFormat[   Tuple1[T1 ]
// ] = new RowFormat[   Tuple1[T1 ]
// ]{
//       def reads (tab:Row,header:Option[IndexedSeq[String]]) = {
//         (
//       fromCell[T1](tab.cells(1-1)))      
        
//       }
//       def writes(tuple:    Tuple2[T1 ],header:Option[IndexedSeq[String]]) = tuple match {
//         case (
//                 t1
//           ) => Row(IndexedSeq(
//         toCell(t1)
//           ))
//         case _ => throw new RuntimeException("Tuple" + 1 + " expected")
//       }
//       def header(tuple:    Tuple2[T1 ]) = Some(IndexedSeq(
//               "V1"    
//         ))
//   }

def asProduct1[S, T1](f1: String)(apply : (T1) => S)(unapply : S => Product1[T1])(implicit bin1: CellFormat[T1]) = new RowFormat[S] {
    def writes(s: S, header: Option[IndexedSeq[String]]) = {
      if (header.isDefined) {
        assert(header.get.toSet == Set(f1), "Headers should be: " + List(f1).mkString(","))
      }
      val product = unapply(s)
      val map = Map(
        header.map(_.indexOf(f1)).getOrElse(1-1) -> toCell(product._1)
     )
      Row(
        map.toSeq.sortBy(_._1).map(_._2).toIndexedSeq)
    }
    def reads(tab: Row, header: Option[IndexedSeq[String]]) = tab match {
      case Row(m) => // m is the IndexedSeq[Cell]
        apply(
          fromCell[T1](m(header.map(_.indexOf(f1)).getOrElse(1-1))))
      case _ => throw new RuntimeException("Row object expected")
    }
    def header(s:S) = Some(IndexedSeq(f1))
  }

   <#list 2..22 as i> 
    <#assign typeParams><#list 1..i-1 as j>T${j}<#if i !=j>,</#if></#list>Seq[T${i}]</#assign>
    <#assign typeParams2><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>

  implicit def tuple${i}SeqFormat[${typeParams2}](implicit 
    <#list 1..i as j>fmt${j}: CellFormat[T${j}]<#if i !=j>,</#if></#list>      
    ): RowFormat[   Tuple${i}[${typeParams} ]
] = new RowFormat[   Tuple${i}[${typeParams} ]
]{
      def reads (tab:Row,header:Option[IndexedSeq[String]]) = {
        (
      <#list 1..i-1 as j>fromCell[T${j}](tab.cells(${j}-1))<#if i !=j>,</#if></#list>
      tab.cells.drop(${i-1}).map(c=>fromCell[T${i}](c))      
        )
      }
      def writes(tuple:    Tuple${i}[${typeParams} ],header:Option[IndexedSeq[String]]) = tuple match {
        case (
                <#list 1..i as j>t${j}<#if i !=j>,</#if></#list>      
          ) => Row(IndexedSeq(
        <#list 1..i-1 as j>toCell(t${j})<#if i-1 !=j>,</#if></#list>      
          ) ++ t${i}.map(x => toCell(x) )
          )
        case _ => throw new RuntimeException("Tuple" + ${i} + " expected")
      }
      def header(tuple:    Tuple${i}[${typeParams} ]) = Some(IndexedSeq(
              <#list 1..i-1 as j>"V${j}"<#if i-1 !=j>,</#if></#list>      
        )++ tuple._${i}.zipWithIndex.map(p => "V"+(p._2+${i}).toString ).toIndexedSeq 
      )
  }
    </#list> 

    // 

  <#list 2..22 as i> 
    <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>

  implicit def tuple${i}Format[${typeParams}](implicit 
    <#list 1..i as j>fmt${j}: CellFormat[T${j}]<#if i !=j>,</#if></#list>      
    ): RowFormat[   Tuple${i}[${typeParams} ]
] = new RowFormat[   Tuple${i}[${typeParams} ]
]{
      def reads (tab:Row,header:Option[IndexedSeq[String]]) = {
        (
      <#list 1..i as j>fromCell[T${j}](tab.cells(${j}-1))<#if i !=j>,</#if></#list>      
        )
      }
      def writes(tuple:    Tuple${i}[${typeParams} ],header:Option[IndexedSeq[String]]) = tuple match {
        case (
                <#list 1..i as j>t${j}<#if i !=j>,</#if></#list>      
          ) => Row(IndexedSeq(
        <#list 1..i as j>toCell(t${j})<#if i !=j>,</#if></#list>      
          ))
        case _ => throw new RuntimeException("Tuple" + ${i} + " expected")
      }
      def header(tuple:    Tuple${i}[${typeParams} ]) = Some(IndexedSeq(
              <#list 1..i as j>"V${j}"<#if i !=j>,</#if></#list>      
        ))
  }
    </#list> 


  def wrap[S, T](f1: String)(apply : T => S)(unapply : S => T)(implicit bin1: CellFormat[T]) = new RowFormat[S] {
    def writes(s: S, header: Option[IndexedSeq[String]]) = {
      if (header.isDefined) {
        assert(header.get.toSet == Set(f1), "Headers should be: " + List(f1).mkString(","))
      }
      val product = unapply(s)      
      Row(
        IndexedSeq(toCell(product))
        )
    }
    def reads(tab: Row, header: Option[IndexedSeq[String]]) = tab match {
      case Row(m) => // m is the IndexedSeq[Cell]
        apply(
          fromCell[T](m.head)
        )
      case _ => throw new RuntimeException("Row object expected")
    }
    def header(s:S) = Some(IndexedSeq(f1))
  }
  
  <#list 2..22 as i> 
  <#assign typeParams><#list 1..i as j>T${j}<#if i !=j>,</#if></#list></#assign>
  <#assign nameStrings><#list 1..i as j>f${j}<#if i != j>,</#if></#list></#assign>
  def asProduct${i}[S, ${typeParams}](<#list 1..i as j>f${j}: String<#if i != j>,</#if></#list>)(apply : (${typeParams}) => S)(unapply : S => Product${i}[${typeParams}])(implicit <#list 1..i as j>bin${j}: CellFormat[T${j}]<#if i != j>,</#if></#list>) = new RowFormat[S] {
    def writes(s: S, header: Option[IndexedSeq[String]]) = {
      if (header.isDefined) {
        assert(header.get.toSet == Set(${nameStrings}), "Headers should be: " + List(${nameStrings}).mkString(","))
      }
      val product = unapply(s)
      val map = Map(
        <#list 1..i as j>header.map(_.indexOf(f${j})).getOrElse(${j}-1) -> toCell(product._${j})<#if i != j>,
        </#if></#list>
      )
      Row(
        map.toSeq.sortBy(_._1).map(_._2).toIndexedSeq)
    }
    def reads(tab: Row, header: Option[IndexedSeq[String]]) = tab match {
      case Row(m) => // m is the IndexedSeq[Cell]
        apply(
          <#list 1..i as j>fromCell[T${j}](m(header.map(_.indexOf(f${j})).getOrElse(${j}-1)))<#if i != j>,
        </#if></#list>
        )
      case _ => throw new RuntimeException("Row object expected")
    }
    def header(s:S) = Some(IndexedSeq(${nameStrings}))
  }
  </#list>
}