import Day7.Rank.{FIVE_OF_A_KIND, FOUR_OF_A_KIND, FULL_HOUSE, HIGH_CARD, ONE_PAIR, THREE_OF_A_KIND, TWO_PAIR}
import Part.Part1

import scala.annotation.tailrec

object Day7:

  type Hand = List[Char]
  type Bet = Int
  case class HandRankBet(hand: Hand, rank: Rank, bet: Bet)

  enum Rank(val order: Int):
    case FIVE_OF_A_KIND extends Rank(1)
    case FOUR_OF_A_KIND extends Rank(2)
    case FULL_HOUSE extends Rank(3)
    case THREE_OF_A_KIND extends Rank(4)
    case TWO_PAIR extends Rank(5)
    case ONE_PAIR extends Rank(6)
    case HIGH_CARD extends Rank(7)

  def run(mode: String, dayNumber: Int): Unit =
    val lines = Utils.getInputLines(mode, dayNumber).toList
    part1(lines)
    part2(lines)

  def solve(lines: List[String], cardValueMap: Map[Char, Int] ,joker:Boolean, part:Part): Long =
    val handBets = lines map parse
    val handRankBets: List[HandRankBet] = handBets.map((hand, bet) => HandRankBet(hand, getRank(hand, joker), bet))

    val tiesBroken = handRankBets
      .groupBy(_.rank)
      .view
      .mapValues(tieBreakOrder(_, cardValueMap))

    val sortedList = tiesBroken.toList.sortBy(_._1.order).flatMap(_._2)
    val allWins = for (s <- sortedList.size - 1 to 0 by -1) yield sortedList(s).bet * (sortedList.size - s)
    val result = allWins.map(_.toLong).sum
    Utils.printResult(part, result.toString)
    result

  def part1(lines: List[String]): Long =
    val cardValueMap: Map[Char, Int] =
      List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zip(2 to 14).toMap

    solve(lines, cardValueMap, false, Part.Part2)


  def part2(lines: List[String]): Long =

    val cardValueMap: Map[Char, Int] =
      List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A').zip(2 to 14).toMap

    solve(lines, cardValueMap,true, Part.Part2)

  def tieBreakOrder(hands: List[HandRankBet], cardValueMap: Map[Char, Int]): Seq[HandRankBet] =

    @tailrec
    def listSorter(hr1:HandRankBet, hr2: HandRankBet): Boolean =
      if(cardValueMap(hr1.hand.head) > cardValueMap(hr2.hand.head)) true
      else if(cardValueMap(hr1.hand.head) < cardValueMap(hr2.hand.head)) false
      else listSorter(
        HandRankBet(hr1.hand.tail, hr1.rank, hr1.bet),
        HandRankBet(hr2.hand.tail, hr1.rank, hr1.bet))

    hands.sortWith(listSorter)

  def getRank(hand: Hand, joker: Boolean = false): Rank =
    val potentialHandMap = hand.groupBy(identity).view.mapValues(_.size).toMap
    val handMap =
      if joker then
        val numberOfJokers = potentialHandMap.getOrElse('J', 0)
        if numberOfJokers < 5 then
          val (highestCard: Char, value) = potentialHandMap.filterNot(_._1 =='J').maxBy(_._2)
          potentialHandMap
            .updated(highestCard, numberOfJokers + value)
            .removed('J')
        else potentialHandMap
      else potentialHandMap

    handMap match
      case hand if hand.size == 1 => FIVE_OF_A_KIND
      case hand if hand.size == 2 && hand.values.max == 4 => FOUR_OF_A_KIND
      case hand if hand.size == 2 && hand.values.max == 3 => FULL_HOUSE
      case hand if hand.size == 3 && hand.values.max == 3 => THREE_OF_A_KIND
      case hand if hand.size == 3 && hand.values.max == 2 => TWO_PAIR
      case hand if hand.size == 4 => ONE_PAIR
      case hand if hand.size == 5 => HIGH_CARD

  def parse(line: String):(Hand, Bet) =
    line match
      case s"$hand $bet" => (hand.toList, bet.toInt)




