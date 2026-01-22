library(flexplot)
source('Deck_gen.R')

sim = function(n, Balance, Prob_deck = c(0.3,0.3,0.3),
               Val_norm = c(seq(0.1,2,0.01)), Prob_norm = c(.98,.012,.008),
               Val_unc = seq(1,3.5,0.01), Prob_unc = c(.965,.023,.012), 
               Val_spec = seq(7,10,0.01), Prob_spec = c(.965,.023,.012)){
  
    start = Sys.time()
  # Initializing a data frame which will contain the parameters of the opened decks
    results = data.frame(NumberOfPack = numeric(), Value = numeric(), Price = numeric(),
                         TypeOfDeck = character(), NormalCards = numeric(), 
                         GoldenCards = numeric(), HoloCards = numeric(),Profit = numeric())
  
  # Iterating through the number of openings 
    for (idx in (1:n)){
      
    TypeOfDeck = sample(c('normal','uncommon','special'), 1, prob = Prob_deck)
    price = check_price(TypeOfDeck)
    deck_list = buy_deck(TypeOfDeck, Balance,
                         Val_norm = Val_norm, Prob_norm = Prob_norm,
                         Val_unc = Val_unc, Prob_unc = Prob_unc,
                         Val_spec = Val_spec, Prob_spec = Prob_spec)
    
    deck = deck_list$deck
    Balance = deck_list$balance
    
    # If deck is indeed NULL, that means that we've got no money, hence the loop is broken
    # and we remain with the already opened packs
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
  
  # We plot out how much have we won or lost 
  print(flexplot(Profit ~ NumberOfPack, data = results, method = 'lm'))
  
  end = Sys.time()
  print(end-start)
  
  return(results)
}

res = sim(n = 100, Balance = 30000, Prob_deck = c(.1,.45,.45))
table(res$TypeOfDeck)


