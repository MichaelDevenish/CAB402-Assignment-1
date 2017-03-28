using Prism.Commands;
using Prism.Interactivity.InteractionRequest;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading.Tasks;
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
            PossibleCards = ComputerPlayer.calculatePossibleDeck(ComputerCards, Discards[Discards.Count-1], deck);
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
                checkReaminingEmpty();
                ButtonEnabled = false;
                Task.Run(() => RunAi());
            }
        }

        async private void RunAi()
        {
            //run AI code here
            PossibleCards = ComputerPlayer.calculatePossibleDeck(ComputerCards, Discards[Discards.Count - 1], PossibleCards);

            checkReaminingEmpty();
            ButtonEnabled = true;
            placedDownCard = true;
        }

        private async void checkReaminingEmpty()
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
                    await Task.Delay(1);
                }
                Discards.Add(newDiscard);
            }
        }

        async private void HumanCards_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            HumanDeadwood = "Calculating ...";
            // this might take a while, so let's do it in the background
            int deadwood = await Task.Run(() => GinRummy.Deadwood(HumanCards));
            HumanDeadwood = "Deadwood: " + deadwood;
            if (deadwood < 10)
            {
                ButtonEnabled = true;
            }
            else
            {
                ButtonEnabled = false;
            }
            if (deadwood == 0)
            {
                ButtonName = "gin";
            }
            else
            {
                ButtonName = "knock";
            }
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
            int score = GinRummy.Score(HumanCards, ComputerCards);
            string result;
            bool final = false;
            if (score < 0)
            {
                score = System.Math.Abs(score);

                if (aiScore + score >= 100)
                {
                    result = "The AI Has won all games with a total score of " + (aiScore + score) +
                        "\n your final score is " + userScore;
                    final = true;
                }
                else
                {
                    result = "The AI won with " + score + " points.";
                    aiScore += score;
                }
            }
            else
            {
                if (userScore + score >= 100)
                {
                    result = "You have won all games with a total score of " + (userScore + score) +
                        "\n the final AI score is " + aiScore;
                    final = true;
                }
                else
                {
                    result = "You won with " + score + " points.";
                    userScore += score;
                }
            }

            if (!final)
            {
                result += "\nYour current total score = " + userScore +
                    "\n The current AI score = " + aiScore;
            }
            else
            {
                userScore = 0;
                aiScore = 0;
            }

            //if there is any cards that the ai has that could make runs when the player knocks (add this to the GinRummy.score algorithim)
            RaiseNotification(result, "Title");
            pickedUpCard = false;
            placedDownCard = true;
            HumanCards.Clear();
            ComputerCards.Clear();
            Discards.Clear();
            RemainingDeck.Clear();
            Deal();
        }
    }
}