library(dplyr)

# We're gonna have 3 types of decks (normal, uncommon, special),
# and 3 type of cards (normal, golden, holo)
# The card types will affect the price of the individual cards (golden - 1.5x, holo - 3x),
# while the deck types will affect the number of cards drawn from the pack,
# also the chance of the respective cards.
# The normal and uncommon decks will have 10 cards each, while the special one will 
# contain only 5 cards, though with a much higher chance of getting a better type of card.

`%notin%` = Negate(`%in%`)

buy_deck = function(deck_type){
  if (deck_type %notin% c('normal','uncommon','special'))
    stop('Type of deck does not exist, you most likely made a typo')
  
  price <<- case_when(deck_type == 'normal' ~ 15,
                      deck_type == 'uncommon' ~ 25,
                      deck_type == 'special' ~ 50)
  # print(paste('Price will be:', price))
  # print(paste('New balance will be:', balance - price))
  
  if (deck_type == 'normal'){
    if (balance < price){stop("You don't have enough money")}
    
    Card_values = sample(seq(0.1,2,0.01),10, replace = T)
    C.type.chance = sample(c('Normal','Golden','Holo'), 10, 
                           prob = c(.98,.012,.008), replace = T)
    
    deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
      mutate(Value = case_when(
        Type == 'Golden' ~ Value * 1.5,
        Type == 'Holo' ~ Value *3,
        .default = Value))
  }
  
  if (deck_type == 'uncommon'){
    if (balance < price){stop("You don't have enough money")}
    
    Card_values = sample(seq(1,3.5,0.01),10, replace = T)
    C.type.chance = sample(c('Normal','Golden','Holo'), 10, 
                           prob = c(.965,.023,.012), replace = T)
    
    deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
      mutate(Value = case_when(
        Type == 'Golden' ~ Value * 1.5,
        Type == 'Holo' ~ Value *3,
        .default = Value))
  }
  
  if (deck_type == 'special'){
    if (balance < price){stop("You don't have enough money")}
    Card_values = sample(seq(7,10,0.01), 5, replace = T)
    C.type.chance = sample(c('Normal','Golden','Holo'), 5, 
                           prob = c(.8,.155,.045), replace = T)
    
    deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
      mutate(Value = case_when(
        Type == 'Golden' ~ Value * 1.5,
        Type == 'Holo' ~ Value *3,
        .default = Value))
  }
  balance <<- balance - price
  return(deck)
}


# simulation
library(flexplot)

sim = function(){
  n = 500
  balance <<- 50000
  results = data.frame(Value = numeric(), Price = numeric(), NumberOfPack = numeric(), TypeOfDeck = character(), 
                       NormalCards = numeric(), GoldenCards = numeric(), HoloCards = numeric(),
                       Balance = numeric(), Profit = numeric())
  for (idx in (1:n)){
    typeofdeck = sample(c('normal','uncommon','special'), 1)
    deck = buy_deck(typeofdeck)
    
    res = data.frame(Value = sum(deck$Value),
                     Price = price,
                     NumberOfPack = idx,
                     TypeOfDeck = typeofdeck,
                     NormalCards = sum(deck$Type == 'Normal'),
                     GoldenCards = sum(deck$Type == 'Golden'),
                     HoloCards = sum(deck$Type == 'Holo'),
                     Balance = balance,
                     Profit = sum(deck$Value) - price)
    results = rbind(results, res)
  }
  
  
  print(colSums(results %>% select(Value, Price, NormalCards, GoldenCards, HoloCards, Profit)))
  flexplot(Profit ~ NumberOfPack, data = results, method = 'lm')
}

sim()
