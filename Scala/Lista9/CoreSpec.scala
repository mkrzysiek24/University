package com.uni.core

import org.scalatest.funsuite.AnyFunSuite

class CardDeckTests extends AnyFunSuite {
  test("Create standard deck") {
    val deck = Deck()
    assert(deck.isStandard)
    assert(deck.cards.size == 52)
  }

  test("Pull card from deck") {
    val deck    = Deck()
    val newDeck = deck.pull()
    assert(newDeck.cards.size == 51)
  }

  test("Push card to deck") {
    val deck    = Deck()
    val card    = Card(Hearts, Ace)
    val newDeck = deck.push(card)
    assert(newDeck.cards.size == 53)
    assert(newDeck.cards.head == card)
  }

  test("Deck contains all suits and ranks") {
    val deck = Deck()
    assert(deck.amountOfColor(Clubs) == 13)
    assert(deck.amountOfColor(Diamonds) == 13)
    assert(deck.amountOfColor(Hearts) == 13)
    assert(deck.amountOfColor(Spades) == 13)
    assert(deck.amountWithFace == 12)
    assert(deck.amountWithNumerical == 36)
  }
}
