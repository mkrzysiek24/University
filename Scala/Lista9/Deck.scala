package com.uni.core

import scala.math.max
import scala.util.Random.shuffle

class Deck(val cards: List[Card]) {
  // Creates a new deck without the first card
  def pull(): Deck =
    cards match {
      case Nil     => throw new IllegalStateException("Deck is empty")
      case x :: xs => new Deck(xs)
    }

  // Creates a new deck with the given card pushed on top
  def push(c: Card): Deck = new Deck(c :: cards)

  // Creates a new deck with a new card (color, value) pushed on top
  def push(color: Color, value: Rank): Deck = new Deck(Card(color, value) :: cards)

  // Checks if the deck is a standard deck
  val isStandard: Boolean = {
    if (cards.size != 52) false
    else cards.toSet == Deck.standardDeck.toSet
  }

  // Amount of duplicates of the given card in the deck
  def duplicatesOfCard(card: Card): Int =
    max(cards.count(_ == card) - 1, 0)

  // Amount of cards in the deck for the given color
  def amountOfColor(color: Color): Int = cards.count(_.color == color)

  // Amount of cards in the deck for the given numerical card (2, 3, 4, 5, 6, 7, 8, 9, 10)
  def amountOfNumerical(numerical: Numerical): Int = cards.count(_.rank == numerical)

  // Amount of all numerical cards in the deck (2, 3, 4, 5, 6, 7, 8, 9, 10)
  val amountWithNumerical: Int = cards.count { card =>
    card.rank match {
      case _: Numerical => true
      case _            => false
    }
  }

  // Amount of cards in the deck for the given face (Jack, Queen & King)
  def amountOfFace(face: Face): Int = cards.count(_.rank == face)

  // Amount of all cards in the deck with faces (Jack, Queen & King)
  val amountWithFace: Int = cards.count { card =>
    card.rank match {
      case _: Face => true
      case _       => false
    }
  }
}

object Deck {
  val standardDeck: List[Card] = {
    for {
      color <- List(Clubs, Diamonds, Hearts, Spades)
      rank  <- (2 to 10).map(Numerical) ++ List(Ace, Jack, Queen, King)
    } yield Card(color, rank)
  }

  // Creates the standard deck with a random order of cards
  def apply(): Deck = new Deck(shuffle(standardDeck))
}
