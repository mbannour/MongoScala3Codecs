package io.github.mbannour.bson.macros

import scala.quoted.*
import scala.reflect.ClassTag

object ClassToCaseFlagMap:

  inline def classToCaseClassMap[T]: Map[Class[?], Boolean] = ${ classToCaseClassMapImpl[T] }

  def classToCaseClassMapImpl[T: Type](using q: Quotes): Expr[Map[Class[?], Boolean]] =

    import quotes.reflect.*

    // A helper map to store primitive to boxed type mappings
    val primitiveTypesMap: Map[TypeRepr, TypeRepr] = Map(
      TypeRepr.of[Boolean] -> TypeRepr.of[java.lang.Boolean],
      TypeRepr.of[Byte] -> TypeRepr.of[java.lang.Byte],
      TypeRepr.of[Char] -> TypeRepr.of[java.lang.Character],
      TypeRepr.of[Double] -> TypeRepr.of[java.lang.Double],
      TypeRepr.of[Float] -> TypeRepr.of[java.lang.Float],
      TypeRepr.of[Int] -> TypeRepr.of[java.lang.Integer],
      TypeRepr.of[Long] -> TypeRepr.of[java.lang.Long],
      TypeRepr.of[Short] -> TypeRepr.of[java.lang.Short]
    )

    // Check if a type is a case class or sealed class
    def isCaseClassOrSealed(tpe: TypeRepr): Boolean =
      tpe.typeSymbol.isClassDef && (tpe.typeSymbol.flags.is(Flags.Case) || tpe.typeSymbol.flags.is(Flags.Sealed))

    // Recursive method to flatten and collect all fields including nested case classes
    def collectFields(tpe: TypeRepr): List[TypeRepr] =
      val paramTypes = tpe.typeSymbol.primaryConstructor.paramSymss.flatten.collect {
        case sym if sym.isTerm && sym.isValDef =>
          sym.termRef.asType match
            case '[f] => TypeRepr.of[f]
      }
      paramTypes.flatMap { fieldType =>
        if isCaseClassOrSealed(fieldType) then collectFields(fieldType) :+ fieldType // Collect nested fields and the class itself
        else List(fieldType) // Primitive or other non-case class type
      }
    end collectFields

    // Flatten type arguments and map primitive types to their boxed equivalents
    def flattenTypeArgs(tpe: TypeRepr): List[TypeRepr] =
      val typeArgs = tpe match
        case AppliedType(_, args) => args
        case _                    => List.empty

      val types = tpe +: typeArgs.flatMap(flattenTypeArgs)
      types.map(t => primitiveTypesMap.getOrElse(t, t))

    // Get all known types (case classes and their fields recursively)
    def getFieldTypes(tpe: TypeRepr): List[TypeRepr] =
      collectFields(tpe) :+ tpe // Include the current type itself

    // Function to create the Map[Class[?], Boolean]
    val flattenedFieldTypes: List[TypeRepr] =
      val mainType = TypeRepr.of[T]
      getFieldTypes(mainType).flatMap(t => flattenTypeArgs(t)) // Recursively flatten fields

    val classToCaseClassEntries: List[Expr[(Class[?], Boolean)]] = flattenedFieldTypes.distinct.map { tpe =>
      tpe.asType match
        case '[tType] =>
          // Summon ClassTag to get runtime class and check if it's a case class or sealed class
          Expr.summon[ClassTag[tType]] match
            case Some(ct) =>
              val classExpr = '{ $ct.runtimeClass.asInstanceOf[Class[?]] }
              val isCaseClassExpr = Expr(isCaseClassOrSealed(tpe))
              '{ $classExpr -> $isCaseClassExpr }
            case None =>
              report.errorAndAbort(s"Cannot summon ClassTag for type: ${tpe.show}")
    }

    // Constructing the map from the entries
    val mapEntriesExpr = Varargs(classToCaseClassEntries)
    '{ Map[Class[?], Boolean](${ mapEntriesExpr }*) }
  end classToCaseClassMapImpl
end ClassToCaseFlagMap
