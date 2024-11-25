package io.github.mbannour.bson.macros

import org.bson.BsonWriter
import org.bson.codecs.EncoderContext

import scala.quoted.*

object CaseClassBsonWriter:
  
  inline def writeCaseClassData[T](className: String, writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit =
    ${ writeCaseClassDataImpl[T]('className, 'writer, 'value, 'encoderContext) }

  def writeCaseClassDataImpl[T: Type](
      className: Expr[String],
      writer: Expr[BsonWriter],
      value: Expr[T],
      encoderContext: Expr[EncoderContext]
  )(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol

    if !typeSymbol.flags.is(Flags.Case) then report.errorAndAbort(s"${typeSymbol.name} is not a case class.")

    val fieldWrites = typeSymbol.caseFields.map { field =>

      val res = AnnotationName.findAnnotationValue[T](Expr(field.name))

      val fieldName: Expr[String] = res match
        case '{ Some($annotationValue: String) } => annotationValue
        case '{ None }                           => Expr(field.name)

      val fieldValueExpr = Select(value.asTerm, field).asExpr

      field.tree.asInstanceOf[ValDef].tpt.tpe.asType match
        case '[String] =>
          '{ $writer.writeString(${ fieldName }, $fieldValueExpr.asInstanceOf[String]) }
        case '[Int] =>
          '{ $writer.writeInt32(${ fieldName }, $fieldValueExpr.asInstanceOf[Int]) }
        case '[Double] =>
          '{ $writer.writeDouble(${ fieldName }, $fieldValueExpr.asInstanceOf[Double]) }
        case '[Boolean] =>
          '{ $writer.writeBoolean(${ fieldName }, $fieldValueExpr.asInstanceOf[Boolean]) }
        case '[Long] =>
          '{ $writer.writeInt64(${ fieldName }, $fieldValueExpr.asInstanceOf[Long]) }
        case '[Option[t]] =>
          '{
            $fieldValueExpr.asInstanceOf[Option[t]] match
              case Some(innerValue) =>
                $writer.writeName(${ fieldName })
                ${ writeOptionField(Type.of[t], 'innerValue, writer, encoderContext) }
              case None =>
                $writer.writeNull(${ fieldName })
          }
        case '[List[t]] =>
          '{
            $writer.writeStartArray(${ fieldName })
            $fieldValueExpr.asInstanceOf[List[t]].foreach { item =>
              ${ writeOptionField(Type.of[t], 'item, writer, encoderContext) }
            }
            $writer.writeEndArray()
          }
        case nestedType =>
          val nestedTypeRepr = nestedType match
            case t: scala.quoted.Type[?] =>
              val convertedTypeRepr = TypeRepr.of(using t)
              convertedTypeRepr
            case _ =>
              report.errorAndAbort("nestedType is not a scala.quoted.Type")
          if nestedTypeRepr.typeSymbol.flags.is(Flags.Case) then
            nestedTypeRepr.asType match
              case '[nt] =>
                '{
                  $writer.writeName(${ fieldName })
                  $writer.writeStartDocument()
                  CaseClassBsonWriter.writeCaseClassData(${ fieldName }, $writer, $fieldValueExpr.asInstanceOf[nt], $encoderContext)
                  $writer.writeEndDocument()
                }
              case _ =>
                report.errorAndAbort(s"Unsupported nested case class type: ${nestedTypeRepr.show}")
          else '{ $writer.writeString(${ fieldName }, $fieldValueExpr.toString) }
          end if
      end match
    }

    Expr.block(fieldWrites.toList, '{ () })
  end writeCaseClassDataImpl

  def writeOptionField[T: Type](fieldType: Type[T], value: Expr[Any], writer: Expr[BsonWriter], encoderContext: Expr[EncoderContext])(using
      Quotes
  ): Expr[Unit] =
    import quotes.reflect.*

    TypeRepr.of[T] match
      case t if t =:= TypeRepr.of[String] =>
        '{ $writer.writeString($value.asInstanceOf[String]) }
      case t if t =:= TypeRepr.of[Int] =>
        '{ $writer.writeInt32($value.asInstanceOf[Int]) }
      case t if t =:= TypeRepr.of[Double] =>
        '{ $writer.writeDouble($value.asInstanceOf[Double]) }
      case t if t =:= TypeRepr.of[Boolean] =>
        '{ $writer.writeBoolean($value.asInstanceOf[Boolean]) }
      case t if t =:= TypeRepr.of[Long] =>
        '{ $writer.writeInt64($value.asInstanceOf[Long]) }
      case t if t <:< TypeRepr.of[Product] && t.typeSymbol.flags.is(Flags.Case) =>
        '{ writeCaseClassData(${ Expr(Type.show[T]) }, $writer, $value.asInstanceOf[T], $encoderContext) }
      case _ =>
        '{ $writer.writeString($value.toString) }
    end match
  end writeOptionField
end CaseClassBsonWriter
