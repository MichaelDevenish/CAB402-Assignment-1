﻿using Prism.Commands;
using Prism.Interactivity.InteractionRequest;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;

namespace QUT
{
    class ViewModel : INotifyPropertyChanged
    {
        public ObservableCollection<Cards.Card> HumanCards { get; private set; }
        public ObservableCollection<Cards.Card> ComputerCards { get; private set; }
        public ObservableCollection<Cards.Card> Discards { get; private set; }
        public ObservableCollection<Cards.Card> RemainingDeck { get; private set; }
        public InteractionRequest<INotification> NotificationRequest { get; private set; }

        private bool pickedUpCard = false;
        private bool placedDownCard = true;
        private Cards.Card discardCard;
        private int aiScore = 0;
        private int userScore = 0;
        private IEnumerable<Cards.Card> PossibleCards;

        public ICommand ButtonCommand { get; set; }
        public ICommand DiscardCardFromHandCommand { get; set; }
        public ICommand TakeCardFromDiscardPileCommand { get; set; }
        public ICommand TakeCardFromDeckCommand { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;

        public ViewModel()
        {
            TakeCardFromDiscardPileCommand = new DelegateCommand<Cards.Card>(TakeCardFromDiscardPile);
            DiscardCardFromHandCommand = new DelegateCommand<Cards.Card>(DiscardCardFromHand);
            TakeCardFromDeckCommand = new DelegateCommand<Cards.Card>(TakeCardFromDeck);

            ButtonCommand = new DelegateCommand(ButtonClick);
            ButtonName = "knock";
            ButtonEnabled = false;

            NotificationRequest = new InteractionRequest<INotification>();

            HumanCards = new ObservableCollection<Cards.Card>();
            ComputerCards = new ObservableCollection<Cards.Card>();
            Discards = new ObservableCollection<Cards.Card>();
            RemainingDeck = new ObservableCollection<Cards.Card>();

            HumanCards.CollectionChanged += HumanCards_CollectionChanged;

            Deal();
        }

        private async void Deal()
        {
            var deck = Cards.Shuffle(Cards.FullDeck);
            foreach (var card in deck)
            {

                RemainingDeck.Add(card);
                await Task.Delay(1);
            }

            for (int i = 0; i < 10; i++)
            {
                ComputerCards.Add(DrawTopCardFromDeck());
                await Task.Delay(30);
                HumanCards.Add(DrawTopCardFromDeck());
                await Task.Delay(30);
            }

            Discards.Add(DrawTopCardFromDeck());
            PossibleCards = CalculatePossibleDeck(ComputerCards, Discards[Discards.Count - 1], deck);
        }



        private Cards.Card DrawTopCardFromDeck()
        {
            var top = RemainingDeck[RemainingDeck.Count - 1];
            RemainingDeck.Remove(top);
            return top;
        }

        private void TakeCardFromDeck(Cards.Card card)
        {
            if (placedDownCard)
            {
                RemainingDeck.Remove(card);
                HumanCards.Add(card);
                pickedUpCard = true;
                placedDownCard = false;
            }

        }

        private void TakeCardFromDiscardPile(Cards.Card p)
        {
            if (placedDownCard)
            {
                Discards.Remove(p);
                HumanCards.Add(p);
                discardCard = p;
                pickedUpCard = true;
                placedDownCard = false;
            }
        }

        private void DiscardCardFromHand(Cards.Card p)
        {
            if (pickedUpCard && (discardCard == null || p != discardCard))
            {
                HumanCards.Remove(p);
                Discards.Add(p);
                pickedUpCard = false;
                discardCard = null;
                RunAi();
            }
        }

        async private void HumanCards_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            HumanDeadwood = "Calculating ...";
            // this might take a while, so let's do it in the background
            int deadwood = await Task.Run(() => GinRummy.Deadwood(HumanCards));
            HumanDeadwood = "Deadwood: " + deadwood;
            ButtonStatus(deadwood);
        }

        private void ButtonStatus(int deadwood)
        {
            if (deadwood <= 10) ButtonEnabled = true;
            else ButtonEnabled = false;
            if (deadwood == 0) ButtonName = "gin";
            else ButtonName = "knock";
        }

        private string humanDeadwood;
        public string HumanDeadwood
        {
            get
            {
                return humanDeadwood;
            }
            private set
            {
                humanDeadwood = value;
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs("HumanDeadwood"));
            }
        }

        private string buttonName;
        public string ButtonName
        {
            get
            {
                return buttonName;
            }
            private set
            {
                buttonName = value;
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs("ButtonName"));
            }
        }

        private bool buttonEnabled;
        public bool ButtonEnabled
        {
            get
            {
                return buttonEnabled;
            }
            private set
            {
                buttonEnabled = value;
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs("ButtonEnabled"));
            }
        }

        private void RaiseNotification(string msg, string title)
        {
            NotificationRequest.Raise(new Notification { Content = msg, Title = title });
        }

        private void ButtonClick()
        {
            win(HumanCards, ComputerCards, false);
        }

        private void win(ObservableCollection<Cards.Card> winner, ObservableCollection<Cards.Card> loser, bool aiOrPlayer)
        {
            int score = GinRummy.Score(winner, loser);
            string result;

            if (score < 0)
            {
                if (!aiOrPlayer) result = AIWin(score);
                else result = PlayerWin(score);
            }
            else
            {
                if (aiOrPlayer) result = AIWin(score);
                else result = PlayerWin(score);
            }

            RaiseNotification(result, "Title");
            pickedUpCard = false;
            placedDownCard = true;
            HumanCards.Clear();
            ComputerCards.Clear();
            Discards.Clear();
            RemainingDeck.Clear();
            Deal();
        }

        private string AIWin(int score)
        {
            string result = "";
            score = System.Math.Abs(score);
            aiScore += score;
            if (aiScore >= 100)
            {
                result = "The AI Has won all games with a total score of " + (aiScore) +
                    "\n your final score is " + userScore;
                userScore = 0;
                aiScore = 0;
            }
            else
            {
                result = "The AI won with " + score + " points.";
                result += "\nYour current total score = " + userScore +
                    "\n The current AI score = " + aiScore;
            }
            return result;
        }

        private string PlayerWin(int score)
        {
            string result = "";
            score = System.Math.Abs(score);
            userScore += score;
            if (userScore >= 100)
            {
                result = "You have won all games with a total score of " + (userScore) +
                    "\n the final AI score is " + aiScore;
                userScore = 0;
                aiScore = 0;
            }
            else
            {
                result = "You won with " + score + " points.";
                result += "\nYour current total score = " + userScore +
                    "\n The current AI score = " + aiScore;
            }
            return result;
        }

        async private void RunAi()
        {
            await Task.Delay(300);
            checkReaminingEmpty();
            Cards.Card discardCard = Discards[Discards.Count - 1];
            PossibleCards = CalculatePossibleDeck(ComputerCards, discardCard, PossibleCards);
            AiPickup(discardCard);
            await Task.Delay(300);
            AiMove();
            await Task.Delay(100);
            checkReaminingEmpty();
            placedDownCard = true;
        }

        ObservableCollection<Cards.Card> CalculatePossibleDeck(ObservableCollection<Cards.Card> computerCards, Cards.Card topDiscard, IEnumerable<Cards.Card> possibleDeckCards)
        {
            ObservableCollection<Cards.Card> tempCards = new ObservableCollection<Cards.Card>();
            foreach (Cards.Card card in possibleDeckCards)
            {
                if (!card.Equals(topDiscard) && !computerCards.Contains(card))
                {
                    tempCards.Add(card);
                }
            }
            return tempCards;
        }

        private void AiMove()
        {
            var move = ComputerPlayer.ComputerMove(ComputerCards);
            if (move.Item1 == ComputerPlayer.Move.Continue)
            {
                ComputerCards.Remove(move.Item2.Value);
                Discards.Add(move.Item2.Value);
            }
            else if (move.Item1 == ComputerPlayer.Move.Gin)
            {
                if (move.Item2.Value != null)
                {
                    ComputerCards.Remove(move.Item2.Value);
                    Discards.Add(move.Item2.Value);
                }
                win(ComputerCards, HumanCards, true);
            }
            else
            {
                ComputerCards.Remove(move.Item2.Value);
                Discards.Add(move.Item2.Value);
                win(ComputerCards, HumanCards, true);
            }
        }

        private void AiPickup(Cards.Card discardCard)
        {
            if (ComputerPlayer.ComputerPickupDiscard(ComputerCards, discardCard, PossibleCards))
            {
                Discards.Remove(discardCard);
                ComputerCards.Add(discardCard);
            }
            else
            {
                ComputerCards.Add(DrawTopCardFromDeck());
            }
        }

        private void checkReaminingEmpty()
        {
            if (RemainingDeck.Count <= 0)
            {
                Cards.Card newDiscard = Discards[Discards.Count - 1];
                Discards.RemoveAt(Discards.Count - 1);
                var temp = Cards.Shuffle(Discards);
                Discards.Clear();
                foreach (var card in temp)
                {
                    RemainingDeck.Add(card);
                    Task.Delay(10);
                }
                Discards.Add(newDiscard);
            }
        }
    }
}