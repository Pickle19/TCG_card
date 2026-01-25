source('Deck_gen.R')
library(dplyr)

sim = function(n, Balance, Prob_deck = c(0.3,0.3,0.3),
               Val_norm = c(seq(0.1,2,0.01)), Prob_norm = c(.98,.012,.008),
               Val_unc = seq(1,3.5,0.01), Prob_unc = c(.965,.023,.012), 
               Val_spec = seq(8.5,12,0.01), Prob_spec = c(.965,.023,.012),
               plot = TRUE){
  
    start = Sys.time()
  # Initializing a data frame which will contain the parameters of the opened decks
    results = data.frame(NumberOfPack = numeric(), Value = numeric(), Price = numeric(),
                         TypeOfDeck = character(), NormalCards = numeric(), 
                         GoldenCards = numeric(), HoloCards = numeric(),Profit = numeric(),
                         IsBroke = logical())
  
  # Iterating through the number of openings 
    for (idx in (1:n)){
    
    IsBroke = FALSE
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
      IsBroke = TRUE
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
  
  results$IsBroke = IsBroke
  # We plot out how much have we won or lost -- OPTIONAL
  if (plot == TRUE){
  print(flexplot::flexplot(Profit ~ NumberOfPack, data = results, method = 'lm'))}
  
  end = Sys.time()
  print(end-start)
  
  return(results)
}

res = sim(n = 30, Balance = 300, Prob_deck = c(.1,.45,.45))

# This function takes an array of players, each of them will be assigned to 
# a simulation. At the end all of the simulations are merged into a single data frame
sim_with_players = function(players,n, balance = c(1500, 3000),
                            prob = c(.2,.4,.4)){
  data = data.frame()

  for (player in players){
    res = sim(n = n, Balance = sample(balance,1,prob = c(.1,.9)), Prob_deck = prob, plot = F)
    res$Name = player
    data = rbind(data, res)
    }
  print(flexplot(Profit~NumberOfPack + TypeOfDeck | Name, data = data, method = 'lm'))
  return(data)
}


players = c('Ari','Kriszti','Mandi','Berci','Rudi','Domi')
d = sim_with_players(players, n = 100, 
                     balance = c(1000,5000))

d %>% group_by(Name) %>%
  summarize(Profit = sum(Profit)) 

d %>% group_by(Name) %>%
  count(TypeOfDeck)


