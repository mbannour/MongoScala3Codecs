package io.github.mbannour.bson.macros

import scala.quoted.*

object CaseClassFactory:

  inline def getInstance[T](fieldData: Map[String, Any]): T = ${ getInstanceImpl[T]('fieldData) }

  def getInstanceImpl[T: Type](fieldData: Expr[Map[String, Any]])(using Quotes): Expr[T] =

    import quotes.reflect.*

    val mainTypeRepr = TypeRepr.of[T]
    val mainTypeSymbol = mainTypeRepr.typeSymbol

    // Ensure the type is a case class
    if !mainTypeSymbol.flags.is(Flags.Case) then
      val errorMsg = s"${mainTypeSymbol.name} is not a case class, and cannot be instantiated this way."
      report.errorAndAbort(errorMsg)

    // Get the primary constructor parameters
    val constructorParams = mainTypeSymbol.primaryConstructor.paramSymss.flatten

    // For each field, build an expression to fetch and cast the value from fieldData
    val fieldExprs: List[Expr[Any]] = constructorParams.map { param =>
      val paramName = param.name
      val paramType = param.tree match
        case vd: ValDef => vd.tpt.tpe

      val keyExpr = AnnotationName.findAnnotationValue[T](Expr(paramName))

      val keyToUse: Expr[String] = keyExpr match
        case '{ Some($annotationValue: String) } => annotationValue
        case '{ None }                           => Expr(paramName)

      paramType.asType match
        case '[Option[t]] =>
          val rawExpr = '{ $fieldData.get($keyToUse) }
          val optionExpr = '{
            $rawExpr match
              case Some(value) => Option(value.asInstanceOf[t])
              case None        => None
          }
          optionExpr

        case '[nestedT] if paramType.typeSymbol.flags.is(Flags.Case) =>
          val rawExpr = '{ $fieldData.getOrElse($keyToUse, throw new RuntimeException(s"Field:${${ Expr(keyToUse.show) }} not found")) }
          val nestedExpr = '{
            $rawExpr match
              case instance: nestedT @unchecked     => instance
              case map: Map[String, Any] @unchecked => getInstance[nestedT](map)
              case other                            => throw new RuntimeException(s"Unexpected type for field  " + other.getClass)
          }

          nestedExpr

        case '[nestedT] if paramType.typeSymbol.flags.is(Flags.Enum) =>
          // Get the fully qualified name of the enum's companion object
          val enumCompanionName = Expr(paramType.typeSymbol.companionModule.fullName)
          val paramNameLiteral = Expr(paramName) // Lift paramName to an Expr for use in the macro-generated code

          // Generate the expression for calling valueOf on the enum's companion object
          val enumExpr = '{
            val rawValue = $fieldData.get($paramNameLiteral)
            rawValue match
              case Some(value: String @unchecked) =>
                try
                  val enumClass = Class.forName($enumCompanionName)
                  val method = enumClass.getMethod("valueOf", classOf[String])
                  method.invoke(enumClass, value).asInstanceOf[nestedT]
                catch
                  case ex: Exception =>
                    throw new RuntimeException("Error decoding enum field '" + ${ Expr(paramName) } + "': " + ex.getMessage, ex)
              case Some(enumValue: nestedT @unchecked) =>
                // Already of the correct enum type

                enumValue
              case None =>
                throw new RuntimeException("Missing enum field: " + ${ Expr(paramName) })
              case other =>
                throw new RuntimeException("Unexpected value type for enum field '" + ${ Expr(paramName) } + "': " + other.getClass)
            end match
          }

          enumExpr

        case '[nestedT] =>
          val castExpr = '{
            val rawValue = $fieldData.getOrElse($keyToUse, throw new RuntimeException(s"Missing field: ${${ Expr(keyToUse.show) }}"))
            try rawValue.asInstanceOf[nestedT]
            catch
              case ex: ClassCastException =>
                throw new RuntimeException(
                  s"Error casting field '${${ Expr(paramName) }}'. Expected: ${${ Expr(paramType.show) }}, Actual: " + rawValue.getClass.getName,
                  ex
                )
          }
          castExpr
      end match
    }

    // Construct an instance of T with the extracted fields
    val instance = Apply(Select(New(TypeIdent(mainTypeSymbol)), mainTypeSymbol.primaryConstructor), fieldExprs.map(_.asTerm)).asExprOf[T]

    instance
  end getInstanceImpl
end CaseClassFactory
