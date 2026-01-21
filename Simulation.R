library(flexplot)
source('Deck_gen.R')

sim = function(n, balance){
  # Initializing a data frame which will contain the parameters of the opened decks
    results = data.frame(NumberOfPack = numeric(), Value = numeric(), Price = numeric(),
                         TypeOfDeck = character(), NormalCards = numeric(), 
                         GoldenCards = numeric(), HoloCards = numeric(),Profit = numeric())
  
  # Iterating through the number of openings 
    for (idx in (1:n)){
      
    TypeOfDeck = sample(c('normal','uncommon','special'), 1)
    price = check_price(TypeOfDeck)
    deck_list = buy_deck(TypeOfDeck, balance)
    deck = deck_list$deck
    balance = deck_list$balance
    
    if (is.null(deck)){
      message('You went broke, simulation is stopped!')
      break}
    
    res = data.frame(NumberOfPack = idx,
                     Value = sum(deck$Value),
                     Price = price,
                     TypeOfDeck = deck_list$type,
                     NormalCards = sum(deck$Type == 'Normal'),
                     GoldenCards = sum(deck$Type == 'Golden'),
                     HoloCards = sum(deck$Type == 'Holo'),
                     Profit = sum(deck$Value) - price)
    results = rbind(results, res)
    }
    
  print(flexplot(Profit ~ NumberOfPack, data = results, method = 'lm'))
  return(results)
}

res = sim(500, 100000)
res


