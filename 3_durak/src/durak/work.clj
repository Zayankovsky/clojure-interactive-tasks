(ns durak.work
  (:use [durak.core :only (run-game)] ))


;;; Your task is to write bot for playing card game "Durak": http://en.wikipedia.org/wiki/Durak
;;; Bot plays against another built-in bot (2 players).
;;; Bot is a map that contains 2 functions: function to attack and function to defend.
;;; It must have following format: {:attack YOUR_ATTACK_FN  :defend YOUR_DEFEND_FN}
;;; Attack and defend functions are similar: they both take same arguments and both must return a card.
;;; Attack function is called when your are an attacker and it should select 1 card from your hand and put it on the table.
;;; Note that if it is not the first card in current attack, it must have the same rank as one of the cards on the table.
;;; If you return nil it means that you don't want to attack and attack is finished.
;;; Defend function is called  when your are a defender and it should select 1 card from your hand and put it on the table.
;;; If you return nil it means you can't (or don't want to) defend and you take all cards from the table.

;;; Card is a map with 2 keys: rank and suit.
;;; Rank is a number from 6 to 14.
;;; 11 - Jack
;;; 12 - Queen
;;; 13 - King
;;; 14 - Ace
;;; Suit is one of the 4 keywords: :spades :hearts :diamonds :clubs
;;; Examples: {:rank 6, :suit :clubs}  {:rank 14, :suit :hearts}

;;; Functions input:
;;; Each function takes single argument - map of following structure (example):
;;; {:hand  [{:rank 6, :suit :clubs}
;;;          {:rank 8, :suit :hearts}],
;;;  :table [{:rank 6, :suit :hearts}],
;;;  :trump :clubs}
;;; Hand is a sequence of cards in your hand.
;;; Table is a sequence of cards already on table.
;;; If you are an attacker and it's start of an attack then table will be empty.
;;; If your are a defender then table will be non-empty and your need to beat last card from the table.
;;; Cards on the table can be represented like: attack  - defense - attack - defense - attack. Odd cards are attack, even cards are defense.
;;; trump is trump suit of the game.

;;; To test your solution call (run-game YOUR_SOLUTION)
;;; Your bot is the player in the lower part of screen (cards are visible to you).
;;; To run next action press SPACE, to restart game press R.
;;; When game is over (one of the players has no cards in his hand) nothing will happen when your press SPACE.
;;; If your press SPACE and nothing happen and game is not over yet, look at stack traces, probably your bot (or built-in bot) tries to perform invalid attack or defense.

(defn attack [{:keys [hand table trump]}]
  (let [allowedranks (if (empty? table) #{6 7 8 9 10 11 12 13 14} (reduce #(conj %1 (%2 :rank)) #{} table))
        allallowedcards (filter #(contains? allowedranks (% :rank)) hand)
        notrumps (remove #(= (% :suit) trump) allallowedcards)
        allowedcards (if (and (empty? notrumps) (empty? table)) allallowedcards notrumps)]
    (if (empty? allowedcards) nil (some #(if (= (% :rank) (apply min (map :rank allowedcards))) %) allowedcards))))

(defn defend [{:keys [hand table trump]}]
  (let [{:keys [suit rank]} (last table)
        allallowedcards (filter #(or (and (= (% :suit) suit) (> (% :rank) rank)) (if (not= suit trump) (= (% :suit) trump))) hand)
        notrumps (remove #(= (% :suit) trump) allallowedcards)
        allowedcards (if (empty? notrumps) allallowedcards notrumps)]
    (if (empty? allowedcards) nil (some #(if (= (% :rank) (apply min (map :rank allowedcards))) %) allowedcards))))

(run-game {:attack attack, :defend defend})

;;; Implement program that takes 2 bots, runs game and return winner.
;;; Study init-game and next-action function in the src/durak/logic.clj. They can be used to run game.



;;; Implement bot that memorizes all cards played in game and use some smarter logic based on probability of opponents cards.
;;; Use clojure's atoms or refs to keep data between moves.
;;; If you've implemented this bot in the first task then you can skip it :)



;;; Implement attack and defense functions that they ask user input. So human can play.
;;; I don't know how to do it, may be some hacks with swing like creating and invoking dialog to ask for user input.



;;; Modify program so that it can play with 3 or 4 players.
