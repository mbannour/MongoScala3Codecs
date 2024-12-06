package io.github.mbannour.mongo.codecs

import com.dimafeng.testcontainers.{ForAllTestContainer, MongoDBContainer}
import io.github.mbannour.bson.macros.CaseClassCodecGenerator
import org.bson.BsonWriter
import org.bson.codecs.Codec
import org.mongodb.scala.*
import org.mongodb.scala.model.Filters
import org.bson.codecs.configuration.CodecRegistries
import org.bson.codecs.configuration.CodecRegistries.fromRegistries
import org.bson.types.ObjectId
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.*
import java.time.{LocalDateTime, ZoneId}
import java.util.UUID

// Case classes for testing
case class Address(street: String, city: String, zipCode: Int)
case class ComplexPerson(
    _id: ObjectId,
    name: String,
    middleName: Option[String], // Optional field
    age: Int,
    height: Double,
    married: Boolean,
    address: Option[Address],
    nicknames: Seq[String],
//                          scores: Map[String, Double],
//                          createdAt: LocalDateTime,
//                          timeZone: ZoneId,
//                          uniqueId: UUID
)

import org.bson._
import org.bson.codecs._
import org.bson.codecs.configuration.CodecRegistry
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.util.Date

class LocalDateTimeCodec extends Codec[LocalDateTime] {

  override def decode(reader: BsonReader, decoderContext: DecoderContext): LocalDateTime = {
    val bsonType = reader.getCurrentBsonType
    if (bsonType != BsonType.DATE_TIME) {
      throw new BsonInvalidOperationException(s"Cannot decode $bsonType to LocalDateTime. Expected DATE_TIME.")
    }
    val dateTime = reader.readDateTime()
    LocalDateTime.ofInstant(new Date(dateTime).toInstant, ZoneOffset.UTC)
  }

  override def encode(writer: BsonWriter, value: LocalDateTime, encoderContext: EncoderContext): Unit = {
    val dateTime = Date.from(value.toInstant(ZoneOffset.UTC))
    writer.writeDateTime(dateTime.getTime)
  }

  override def getEncoderClass: Class[LocalDateTime] = classOf[LocalDateTime]
}


class CodecProviderMacroIntegrationSpec extends AnyFlatSpec with ForAllTestContainer with Matchers with ScalaFutures with BeforeAndAfterAll:

  override val container: MongoDBContainer = MongoDBContainer("mongo:6.0.19")

  "CodecProviderMacro" should "create a valid codec provider for a complex nested case class with additional types and optional fields" in {

    // Ensure the MongoDB container is running
    assert(container.container.isRunning, "The MongoDB container is not running!")

    // Construct the MongoDB URI
    val mongoUri = s"mongodb://${container.containerIpAddress}:${container.mappedPort(27017)}"
    val mongoClient: MongoClient = MongoClient()
    val database: MongoDatabase = mongoClient.getDatabase("test_db")

    val codecProvider1 = CodecProviderMacro.createCodecProvider[Address](
      MongoClient.DEFAULT_CODEC_REGISTRY,
      encodeNone = true
    )

    val addressCoded: Codec[Address] = CaseClassCodecGenerator.generateCodec[Address](MongoClient.DEFAULT_CODEC_REGISTRY, true)
    // Create a custom CodecRegistry with the generated codec for the ComplexPerson case class
    val codecProvider = CodecProviderMacro.createCodecProvider[ComplexPerson](
      CodecRegistries.fromRegistries(CodecRegistries.fromCodecs(List(addressCoded).asJava), CodecRegistries.fromCodecs(new LocalDateTimeCodec), MongoClient.DEFAULT_CODEC_REGISTRY),
      encodeNone = true
    )

    val codecRegistry = CodecRegistries.fromRegistries(
      CodecRegistries.fromProviders(codecProvider), // Your custom provider
      CodecRegistries.fromCodecs(new LocalDateTimeCodec),
      MongoClient.DEFAULT_CODEC_REGISTRY // Ensure fallback for built-in types
    )

    // Use the custom codec registry in the database
    val customDatabase: MongoDatabase = database.withCodecRegistry(codecRegistry)
    val collection: MongoCollection[ComplexPerson] = customDatabase.getCollection("complex_people")

    // Create a ComplexPerson object with optional and other fields
    val complexPerson = ComplexPerson(
      _id = new ObjectId(),
      name = "Alice",
      middleName = Some("Marie"), // Optional field with value
      age = 30,
      height = 5.6,
      married = true,
      address = Some(Address("123 Main St", "Wonderland", 12345)),
      nicknames = Seq("Ally", "Lissie"),
      //      scores = Map("math" -> 95.5, "science" -> 89.0),
//            createdAt = LocalDateTime.now(),
      //      timeZone = ZoneId.systemDefault(),
//            uniqueId = UUID.randomUUID()
    )

    // Insert the complex object into the collection
    collection.insertOne(complexPerson).toFuture().futureValue

    // Retrieve the object back from the collection
    val retrievedComplexPerson = collection.find(Filters.equal("_id", complexPerson._id)).first().toFuture().futureValue

//    def test(b: BsonWriter) = {
//      b.writeDouble()
//    }
//

   //  Assertions to ensure the retrieved object matches the original
    retrievedComplexPerson shouldBe complexPerson
//
//    // Test with None for optional field
//    val complexPersonWithoutMiddleName = complexPerson.copy(middleName = None, _id = new ObjectId())
//    collection.insertOne(complexPersonWithoutMiddleName).toFuture().futureValue
//
//    val retrievedPersonWithoutMiddleName =
//      collection.find(Filters.eq("_id", complexPersonWithoutMiddleName._id)).first().toFuture().futureValue
//    retrievedPersonWithoutMiddleName shouldBe complexPersonWithoutMiddleName
//
//     // Close the MongoDB connection
//    customDatabase.drop().toFuture().futureValue
//    mongoClient.close()
  }

  override def afterAll() = container.stop()
end CodecProviderMacroIntegrationSpec
