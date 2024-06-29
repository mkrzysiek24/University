package com.uni.blackjack

import com.uni.core._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlackjackGameSpec extends AnyFlatSpec with Matchers {

  "BlackjackGame" should "calculate points correctly for Ace" in {
    val ace       = Card(Clubs, Ace)
    val blackjack = new BlackjackGame(new Deck(Deck.standardDeck))
    val points1   = blackjack.calculatePoints(ace, 0)
    val points2   = blackjack.calculatePoints(ace, 10)
    points1 should (be(11) or be(1))
    points2 should be(11)
  }

  it should "calculate points correctly for a numerical card" in {
    val numericalCard = Card(Clubs, Numerical(7))
    val blackjack     = new BlackjackGame(new Deck(Deck.standardDeck))
    val points        = blackjack.calculatePoints(numericalCard, 0)
    points should be(7)
  }

  it should "calculate points correctly for a face card" in {
    val faceCard  = Card(Clubs, Jack)
    val blackjack = new BlackjackGame(new Deck(Deck.standardDeck))
    val points    = blackjack.calculatePoints(faceCard, 0)
    points should be(10)
  }

  it should "find all subsequences of cards that sum up to 21" in {
    val deck = List(
      Card(Clubs, Numerical(2)),
      Card(Clubs, Numerical(9)),
      Card(Clubs, Numerical(8)),
      Card(Diamonds, Numerical(3)),
      Card(Diamonds, Numerical(4))
    )
    val blackjack = new BlackjackGame(new Deck(deck))
    val all21     = blackjack.all21
    all21.length should be > 0
    val firstSequence = all21.head.map(_.rank).toSet
    firstSequence should contain theSameElementsAs Set(Numerical(4), Numerical(8), Numerical(9))
  }

  it should "find and print the first subsequence of cards that sum up to 21" in {
    val deck = List(
      Card(Clubs, Numerical(2)),
      Card(Clubs, Numerical(9)),
      Card(Clubs, Numerical(8)),
      Card(Diamonds, Numerical(3)),
      Card(Diamonds, Numerical(4))
    )
    val blackjack = new BlackjackGame(new Deck(deck))

    // Redirect console output for testing purposes
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      blackjack.first21()
    }
    output.toString.trim should include("Total points: 21")
  }

  it should "simulate playing blackjack with a given number of cards" in {
    val deck = List(
      Card(Clubs, Numerical(2)),
      Card(Clubs, Numerical(9)),
      Card(Clubs, Numerical(8)),
      Card(Diamonds, Numerical(3)),
      Card(Diamonds, Numerical(4))
    )
    val blackjack = new BlackjackGame(new Deck(deck))

    // Redirect console output for testing purposes
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      blackjack.play(3)
    }
    output.toString.trim should include("Total points:")
  }
}
