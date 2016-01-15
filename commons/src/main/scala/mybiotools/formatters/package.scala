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

package mybiotools

import Gender._

package object formatters {

  def inverseBoolean(x: Option[Boolean]) = x match {
    case None => None
    case Some(y) => y match {
      case true => Some(false)
      case false => Some(true)
    }
  }

  def boolFromString(s: String) = s match {
    case x if x.toLowerCase == "true" || x.toLowerCase == "yes" || x.toLowerCase == "t" || x == "1" => Some(true)
    case x if x.toLowerCase == "false" || x.toLowerCase == "no" || x.toLowerCase == "f" || x == "0" => Some(false)
    case _ => None
  }

  def parseOptionalDoubleFromString(s: String, format: Format): Option[Double] = s match {
    case x if x == format.missingValue => None
    case x => try { Some(x.toDouble) } catch { case _: Throwable => None }
  }

  trait Format {
    def missingValue: String
    def separator: String
  }

  object PlinkCovar extends Format {
    val missingValue = "-9"
    val separator = " "
  }
  object PlinkFam extends Format {
    val missingValue = "-9"
    val separator = " "
  }

  object RFormat extends Format {
    val missingValue = "NA"
    val separator = ","
  }
  object EmmaxFormat extends Format {
    val missingValue = "NA"
    val separator = " "
  }

  trait Formatter[-T, +F <: Format] {

    def formattedObject(x: T): String

    def formattedHeader(x: T, k: Any): String = k match {
      case k: Symbol => k.name
      case k: Any => k.toString
    }

  }

  def format[T, F <: Format](x: T)(implicit formatter: Formatter[T, F]) = formatter.formattedObject(x)

  def format[T, F <: Format](x: T, f: F)(implicit formatter: Formatter[T, F]) = formatter.formattedObject(x)

  def formatHeader[T, F <: Format](x: T, header: Any)(implicit formatter: Formatter[T, F]) = formatter.formattedHeader(x, header)

  case class CategoricalData(value: Option[Symbol], valueSet: Seq[Symbol]) {

    val width = valueSet.size

  }

  private[formatters] def anyToString(x: Any) = x match {
    case x: Symbol => x.name
    case x: Any => x.toString
  }

  def formatTable[A, F <: Format](
    data: FlatTable[A, Any],
    software: F = RFormat,
    keys: List[A] = Nil,
    quoteString: Boolean = false,
    writeHeaderLine: Boolean = true
  )(implicit f: Formatter[Any, F]): String = {

    val sep = software.separator

    def getKeys(data: FlatTable[A, Any]): Set[A] = {
      if (data.size == 0)
        Set()
      else
        data.head.keySet union getKeys(data.tail)
    }

    def format[T](cell: T)(implicit abc: Formatter[T, F]): String = {
      val x = formatters.format[T, F](cell)
      if (quoteString) cell match {
        case y: String => "\"" + x + "\""
        case _ => x
      }
      else x
    }

    def keyToString(key: A) = key match {
      case x: Symbol => x.name
      case x: Any => x.toString
    }

    def formatHeader(key: A): String = {
      data.find(_.contains(key)) match {
        case None => keyToString(key)
        case Some(x) => {
          val firstCell = x(key)
          formatters.formatHeader(firstCell, key)
          // if ( firstCell.isInstanceOf[FormattableData] ) {
          //   firstCell.asInstanceOf[FormattableData].formatHeader( key, software )
          // } else keyToString( key )

        }
      }
    }

    val missingValue = software.missingValue

    val keysToWrite = if (keys.size > 0) keys else getKeys(data).toList.sortBy(_.toString)

    val listOfRows = for (row <- data) yield {
      val listOfElems = for (key <- keysToWrite) yield {
        if (row.contains(key)) format(row(key)) else missingValue
      }
      listOfElems.mkString(sep)
    }

    val body = (listOfRows.filter(_.size > 0)).mkString("\n")

    if (writeHeaderLine) {
      val keyLine = keysToWrite.map(x => formatHeader(x)).mkString(sep)
      if (body != "")
        List(keyLine, body).mkString("\n")
      else
        keyLine
    } else
      body
  }

  object Formatters extends LowPriorityFormatters {

    implicit object RFormatter extends Formatter[Any, RFormat.type] {
      def formattedObject(x: Any) = x match {
        case x: Option[_] => AnyRFormatter.formattedObject(x)
        case x: Gender => GenderRFormatter.formattedObject(x)
        case x: Boolean => BinaryRFormatter.formattedObject(x)
        case x: CategoricalData => CategoricalRFormatter.formattedObject(x)
        case x => GenericRFormatter.formattedObject(x)
      }

      override def formattedHeader(x: Any, k: Any) = x match {
        case y: CategoricalData => CategoricalRFormatter.formattedHeader(y, k)
        case _ => super.formattedHeader(x, k)
      }

    }

    implicit object PlinkFamFormatter extends Formatter[Any, PlinkFam.type] {
      def formattedObject(x: Any) = x match {
        case x: Option[_] => AnyPlinkFamFormatter.formattedObject(x)
        case x: Gender => GenderPlinkFamFormatter.formattedObject(x)
        case x: Boolean => BinaryPlinkFamFormatter.formattedObject(x)
        case x => GenericPlinkFamFormatter.formattedObject(x)
      }
    }

    implicit object PlinkCovarFormatter extends Formatter[Any, PlinkCovar.type] {
      def formattedObject(x: Any) = x match {
        case x: Option[_] => AnyPlinkCovarFormatter.formattedObject(x)
        case x: Gender => GenderPlinkCovarFormatter.formattedObject(x)
        case x: Boolean => BinaryPlinkCovarFormatter.formattedObject(x)
        case x: CategoricalData => CategoricalPlinkCovarFormatter.formattedObject(x)

        case x => GenericPlinkCovarFormatter.formattedObject(x)
      }

      override def formattedHeader(x: Any, k: Any) = x match {
        case y: CategoricalData => CategoricalPlinkCovarFormatter.formattedHeader(y, k)
        case _ => super.formattedHeader(x, k)
      }
    }

    implicit object EmmaxFormatter extends Formatter[Any, EmmaxFormat.type] {
      def formattedObject(x: Any) = x match {
        case x: Option[_] => AnyEmmaxFormatter.formattedObject(x)
        case x: Gender => GenderEmmaxFormatter.formattedObject(x)
        case x: Boolean => BinaryEmmaxFormatter.formattedObject(x)
        case x: CategoricalData => CategoricalEmmaxFormatter.formattedObject(x)

        case x => GenericEmmaxFormatter.formattedObject(x)
      }

      override def formattedHeader(x: Any, k: Any) = x match {
        case y: CategoricalData => CategoricalEmmaxFormatter.formattedHeader(y, k)
        case _ => super.formattedHeader(x, k)
      }
    }
  }

  class LowPriorityFormatters extends Low2 {

    implicit object AnyRFormatter extends Formatter[Option[Any], RFormat.type] {
      def formattedObject(x: Option[Any]) = x match {
        case None => NoneRFormatter.formattedObject(None)
        case Some(x) => x match {
          case x: Boolean => format(x, RFormat)(BinaryRFormatter)
          case x: Gender => format(x, RFormat)(GenderRFormatter)
          case x => format(x, RFormat)(GenericRFormatter)
        }
      }
    }

    implicit object AnyPlinkFamFormatter extends Formatter[Option[Any], PlinkFam.type] {
      def formattedObject(x: Option[Any]) = x match {
        case None => NonePlinkFamFormatter.formattedObject(None)
        case Some(x) => x match {
          case x: Boolean => format(x, PlinkFam)(BinaryPlinkFamFormatter)
          case x: Gender => format(x, PlinkFam)(GenderPlinkFamFormatter)
          case x => format(x, PlinkFam)(GenericPlinkFamFormatter)
        }
      }
    }

    implicit object AnyPlinkCovarFormatter extends Formatter[Option[Any], PlinkCovar.type] {
      def formattedObject(x: Option[Any]) = x match {
        case None => NonePlinkCovarFormatter.formattedObject(None)
        case Some(x) => x match {
          case x: Boolean => format(x, PlinkCovar)(BinaryPlinkCovarFormatter)
          case x: Gender => format(x, PlinkCovar)(GenderPlinkCovarFormatter)
          case x => format(x, PlinkCovar)(GenericPlinkCovarFormatter)
        }
      }
    }

    implicit object AnyEmmaxFormatter extends Formatter[Option[Any], EmmaxFormat.type] {
      def formattedObject(x: Option[Any]) = x match {
        case None => NoneEmmaxFormatter.formattedObject(None)
        case Some(x) => x match {
          case x: Boolean => format(x, EmmaxFormat)(BinaryEmmaxFormatter)
          case x: Gender => format(x, EmmaxFormat)(GenderEmmaxFormatter)
          case x => format(x, EmmaxFormat)(GenericEmmaxFormatter)
        }
      }
    }

    implicit object CategoricalRFormatter extends Formatter[CategoricalData, RFormat.type] {
      def formattedObject(x: CategoricalData) = x.value match {
        case None => RFormat.missingValue
        case Some(y) => y.name
      }
    }

    implicit object CategoricalPlinkCovarFormatter extends Formatter[CategoricalData, PlinkCovar.type] {
      def formattedObject(x: CategoricalData) = x.value match {
        case None => (1 to (x.width - 1)).map(x => PlinkCovar.missingValue).mkString(PlinkCovar.separator)
        case Some(v) => (1 to (x.width - 1)).map(y => if (v == x.valueSet(y - 1)) "1" else "0").mkString(PlinkCovar.separator)
      }

      override def formattedHeader(x: CategoricalData, k: Any): String = (1 to (x.width - 1)).map(y => anyToString(k) + "_" + x.valueSet(y - 1).name).mkString(PlinkCovar.separator)
    }

    implicit object CategoricalEmmaxFormatter extends Formatter[CategoricalData, EmmaxFormat.type] {
      def formattedObject(x: CategoricalData) = x.value match {
        case None => (1 to (x.width - 1)).map(x => EmmaxFormat.missingValue).mkString(EmmaxFormat.separator)
        case Some(v) => (1 to (x.width - 1)).map(y => if (v == x.valueSet(y - 1)) "1" else "0").mkString(EmmaxFormat.separator)
      }

      override def formattedHeader(x: CategoricalData, k: Any): String = (1 to (x.width - 1)).map(y => anyToString(k) + "_" + x.valueSet(y - 1).name).mkString(EmmaxFormat.separator)
    }

    object NoneRFormatter extends Formatter[None.type, RFormat.type] {
      def formattedObject(x: None.type) = RFormat.missingValue
    }

    object NonePlinkFamFormatter extends Formatter[None.type, PlinkFam.type] {
      def formattedObject(x: None.type) = PlinkFam.missingValue
    }

    object NonePlinkCovarFormatter extends Formatter[None.type, PlinkCovar.type] {
      def formattedObject(x: None.type) = PlinkCovar.missingValue
    }

    object NoneEmmaxFormatter extends Formatter[None.type, EmmaxFormat.type] {
      def formattedObject(x: None.type) = EmmaxFormat.missingValue
    }

    implicit object BinaryRFormatter extends Formatter[Boolean, RFormat.type] {
      def formattedObject(x: Boolean) = x match {
        case true => "T"
        case false => "F"
      }
    }
    implicit object BinaryPlinkCovarFormatter extends Formatter[Boolean, PlinkCovar.type] {
      def formattedObject(x: Boolean) = x match {
        case true => "1"
        case false => "0"
      }
    }
    implicit object BinaryPlinkFamFormatter extends Formatter[Boolean, PlinkFam.type] {
      def formattedObject(x: Boolean) = x match {
        case true => "2"
        case false => "1"
      }
    }
    implicit object BinaryEmmaxFormatter extends Formatter[Boolean, EmmaxFormat.type] {
      def formattedObject(x: Boolean) = x match {
        case true => "1"
        case false => "0"
      }
    }

    implicit object GenderRFormatter extends Formatter[Gender, RFormat.type] {
      def formattedObject(x: Gender) = x match {
        case Male => "Male"
        case Female => "Female"
      }
    }
    implicit object GenderPlinkCovarFormatter extends Formatter[Gender, PlinkCovar.type] {
      def formattedObject(x: Gender) = x match {
        case Male => "1"
        case Female => "0"
      }
    }
    implicit object GenderPlinkFamFormatter extends Formatter[Gender, PlinkFam.type] {
      def formattedObject(x: Gender) = x match {
        case Male => "1"
        case Female => "2"
      }
    }
    implicit object GenderEmmaxFormatter extends Formatter[Gender, EmmaxFormat.type] {
      def formattedObject(x: Gender) = x match {
        case Male => "1"
        case Female => "0"
      }
    }
  }

  class Low2 {
    implicit object GenericRFormatter extends Formatter[Any, RFormat.type] {
      def formattedObject(x: Any) = x.toString
    }

    implicit object GenericPlinkFamFormatter extends Formatter[Any, PlinkFam.type] {
      def formattedObject(x: Any) = x.toString
    }

    implicit object GenericPlinkCovarFormatter extends Formatter[Any, PlinkCovar.type] {
      def formattedObject(x: Any) = x.toString
    }

    implicit object GenericEmmaxFormatter extends Formatter[Any, EmmaxFormat.type] {
      def formattedObject(x: Any) = x.toString
    }
  }

}