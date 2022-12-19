package MarkB_FinalProject.rockpaperscissors

import fpinscala.parsing.JSON.{JArray, JBool, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}
import benjamingarrett.rockpaperscissorstools.{RPSHistoryBasedPlayer}

import scala.io.Source._

object TournamentSettingsDeserializer {
  def go()= {
    val fileName = "src/main/resources/tournamentConfig.json"
    val fileContents1 = io.Source.fromFile(fileName)
    val fileContents2 = fileContents1.getLines().mkString

    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParse = P.run(json)(fileContents2)
    resultOfParse.flatMap(j => unpacker(j)).map(dto => printDTO(dto))
  }

  //  def secondParse(target: String): Either[ParseError, List[nonBiasedPlayerDTO]]={
  //    val P = fpinscala.parsing.Reference
  //    import fpinscala.parsing.ReferenceTypes.Parser
  //    val json: Parser[JSON] = JSON.jsonParser(P)
  //    val resultOfParse2 = P.run(json)(target)
  //
  //    resultOfParse2.flatMap(j => unpacker2(j))
  //  }

  case class tournamentDTO(
                            tournaments: Double,
                            roundsPerMatch: Double,
                            players: List[playerTemplateDTO]
                          )

  case class playerTemplateDTO(
                                name: String,
                                playerType: String,
                                rockWeight: Double,
                                paperWeight: Double,
                                scissorWeight: Double
                              )

  case class nonBiasedPlayerDTO(
                                 nonBname: String,
                                 nonBplayerType: String
                               )

  case class BiasedPlayerWeightsDTO(
                                     rockWeight: Double,
                                     paperWeight: Double,
                                     scissorWeight: Double
                                   )

  def unpacker(json: JSON): Either[ParseError, tournamentDTO] = {

    //val players = List(playerTemplateDTO("BiasedRandomPlayer", "BiasedRandomMovePlayer", 0.3, 0.3, 0.4))
    json match {
      case jObject: JObject =>
        for {
          tournaments <- unpackNumber(jObject, "tournaments")
          roundsPerMatch <- unpackNumber(jObject, "roundsPerMatch")
          player1 <- unpackPlayer(jObject, "player1")
          player2 <- unpackPlayer(jObject, "player2")
          player3 <- unpackPlayer(jObject, "player3")
          player4 <- unpackPlayer(jObject, "player4")
          player5 <- unpackPlayer(jObject, "player5")
          player6 <- unpackPlayer(jObject, "player6")

          players = List(player1, player2, player3, player4, player5, player6)
        } yield tournamentDTO(tournaments, roundsPerMatch, players)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
    }
  }

  def unpackPlayer(jObject: JObject, key: String): Either[ParseError, playerTemplateDTO]= jObject.get(key)match {
    case jObject2: JObject =>
      for{
        name <- unpackString(jObject2, "name")
        playerType <- unpackString(jObject2, "type")
      } yield playerTemplateDTO(name, playerType, 0, 0, 0)
    case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
  }

  def unpackPLayer2(jObject: JObject, key: String): Either[ParseError, playerTemplateDTO] = jObject.get(key) match {
    case jObject2: JObject =>
      for {
        name <- unpackString(jObject2, "name")
        playerType <- unpackString(jObject2, "type")
        rockWeight <- unpackNumber(jObject2, "rock")
        paperWeight <- unpackNumber(jObject2, "paper")
        scissorsWeight <- unpackNumber(jObject2, "scissors")
      } yield playerTemplateDTO(name, playerType, rockWeight, paperWeight, scissorsWeight)
    case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
  }
  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {

    case jNumber: JNumber => Right(jNumber.get)
    case _ => Left(ParseError(List((Location("Could not unpack this number"), "double"))))
  }

  def unpackString(jObject: JObject, key: String): Either[ParseError, String] = jObject.get(key) match {
    case jString: JString => Right(jString.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }
//
//  def unpackPlayer1(json: JSON): Either[ParseError, nonBiasedPlayerDTO] = {
//    json match {
//      case jObject: JObject =>
//        for {
//          name <- unpackString(jObject, "name")
//          playerType <- unpackString(jObject, "type")
//        } yield nonBiasedPlayerDTO(name, playerType)
//      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
//    }
//  }

//  def unpackBiasedPlayer(jObject: JObject, target: String): Either[ParseError, BiasedPlayerWeightsDTO] = jObject.get(target) match {
//    case jObject2: JObject =>
//      for {
//        rockWeight <- unpackNumber(jObject2, "rock")
//        paperWeight <- unpackNumber(jObject2, "paper")
//        scissorWeight <- unpackNumber(jObject2, "scissors")
//      } yield BiasedPlayerWeightsDTO(rockWeight, paperWeight, scissorWeight)
//    case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
//  }


//  def unpackList(c: List[JSON], e: Either[ParseError, List[nonBiasedPlayerDTO]]): Either[ParseError, List[nonBiasedPlayerDTO]] = {
//
////    c match {
////      case ::(head, next) => head match {
////        case jObject:JObject => jObject for {
////          playerName <- unpackString(jObject, "name")
////          playertype <- unpackString(jObject, "type")
////          list = nonBiasedPlayerDTO(playerName, playertype)
////        } yield unpackList(next, r.flatMap(l => Right(list :: l)))
////        case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
////      }
////      case Nil => r
////      }
//    if(c == List.empty){
//      Either[ParseError, Nil]
//    }
//    else {
//      unpackList(c, e)
//    }
//  }

//        c match {
//          case ::(head, next) => head match {
//            case jObject: JObject => {
//              for {
//                name <- unpackString(jObject, "name")
//                playerType <- unpackString(jObject, "type")
//                weights <- unpackBiasedPlayer(jObject, "weights")
//              } //yield playerTemplateDTO(name, playerType, weights.rockWeight, weights.paperWeight, weights.scissorWeight)
//              yield unpackList(next, r.flatMap(list => Right(playerTemplateDTO(name, playerType, weights.rockWeight, weights.paperWeight, weights.scissorWeight) :: list)))
//            }
//            case _ => Left(ParseError(List((Location("Could not unpack the player array"), "array"))))
//          }
//        }



//  def unpackArray(jObject: JObject, key: String): Either[ParseError, List[Object]] = {
//    for {
//      playersPacked <- jObject.get(key) match {
//        case jArray: JArray => Right(jArray.get)
//        case _ => Left(ParseError(List((Location("Could not unpack the player array"), "array"))))
//      }
//      players <- unpackList(playersPacked.toList, Right(List.empty))
//    } yield players
//  }

    def printDTO(tourndto: tournamentDTO):Unit = {
      println("Tournaments: " + tourndto.tournaments)
      println("Rounds Per Match: " + tourndto.roundsPerMatch)
      println("Players in this season: ")
      for(p <- tourndto.players){
        println("")
        println("  name: " + p.name)
        println("  type: " + p.playerType)
        println("  Rockweight: " + p.rockWeight)
        println("  Paperweight: "+p.paperWeight)
        println("  Scissorweight: "+p.scissorWeight)

      }
    }



}
object ProofItWorks extends App{
  TournamentSettingsDeserializer.go()
}