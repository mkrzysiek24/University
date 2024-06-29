package com.uni.blackjack

import com.uni.core._

import scala.util.Random.shuffle

class BlackjackGame(deck: Deck) {
  // Points calculation:
  // Numerical cards as tsheir numerical value = 2 - 10.
  // Face cards (Jack, Queen, King) = 10
  // Ace = 1 or 11 (player could choose)
  def calculatePoints(card: Card, currentPoints: Int): Int = card.rank match {
    case Numerical(value) => value
    case _: Face          => 10
    case Ace              => if (currentPoints + 11 <= 21) 11 else 1
  }

  // Loop taking n cards from the deck, pretty-printing them with points & printing the sum of points at the end
  def play(n: Int): Unit = {
    println(s"Playing Blackjack with $n cards:")
    val (selectedCards, _) = deck.cards.splitAt(n)

    def playHelper(cards: List[Card], accumulatedPoints: Int): Int = cards match {
      case Nil => accumulatedPoints
      case card :: remainingCards =>
        val points = calculatePoints(card, accumulatedPoints)
        println(s"${card.color} ${card.rank} - Points: $points")
        playHelper(remainingCards, accumulatedPoints + points)
    }

    val totalPoints = playHelper(selectedCards, 0)
    println(s"Total points: $totalPoints")
  }

  // Finds all subsequences of cards which could give 21 points
  lazy val all21: List[List[Card]] = {
    def find21(
        remaining: List[Card],
        current: List[Card],
        result: List[List[Card]]
    ): List[List[Card]] = {
      val currentPoints = current.map(c => calculatePoints(c, 0)).sum
      if (currentPoints == 21)
        result :+ current
      else if (currentPoints < 21 && remaining.nonEmpty)
        find21(remaining.tail, remaining.head :: current, result) ++
          find21(remaining.tail, current, result)
      else
        result
    }
    find21(deck.cards, Nil, Nil)
  }

  // Finds and pretty-prints the first subsequence of cards which could give 21 points
  def first21(): Unit = {
    println("First subsequence of cards:")
    all21.headOption match {
      case Some(cards) =>
        cards.foreach(card => println(s"${card.color} ${card.rank}"))
        println("Total points: 21")
      case None => println("No subsequence of cards.")
    }
  }
}

object BlackjackGame {
  // Creates Blackjack game having numOfDecks amount of standard decks with random order of cards. For example, with BlackjackGame(3) deck would have 156 cards
  def apply(numOfDecks: Int): BlackjackGame = {
    val fullDeck = (1 to numOfDecks).flatMap(_ => Deck.standardDeck)
    new BlackjackGame(new Deck(shuffle(fullDeck.toList)))
  }
}
