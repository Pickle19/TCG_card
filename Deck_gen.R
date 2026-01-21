library(dplyr)

# We're gonna have 3 types of decks (normal, uncommon, special),
# and 3 type of cards (normal, golden, holo)
# The card types will affect the price of the individual cards (golden - 1.5x, holo - 3x),
# while the deck types will affect the number of cards drawn from the pack,
# also the chance of the respective cards.
# The normal and uncommon decks will have 10 cards each, while the special one will 
# contain only 5 cards, though with a much higher chance of getting a better type of card.

`%notin%` = Negate(`%in%`)

# ---------------------------------------------

check_price = function(deck_type){
  price = case_when(deck_type == 'normal' ~ 15,
                    deck_type == 'uncommon' ~ 25,
                    deck_type == 'special' ~ 50)
  return(price)
}

# ---------------------------------------------

gen_normal_deck = function(){
  # Simulating the cards
  Card_values = sample(seq(0.1,2,0.01),10, replace = T)
  C.type.chance = sample(c('Normal','Golden','Holo'), 10, 
                         prob = c(.98,.012,.008), replace = T)
  # Assembling the deck
  deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
    mutate(Value = case_when(
      Type == 'Golden' ~ Value * 1.5,
      Type == 'Holo' ~ Value *3,
      .default = Value))
  
  return(deck)
}

# --------------------------------------------

gen_uncommon_deck = function(){
  Card_values = sample(seq(1,3.5,0.01),10, replace = T)
  C.type.chance = sample(c('Normal','Golden','Holo'), 10, 
                         prob = c(.965,.023,.012), replace = T)
  
  deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
    mutate(Value = case_when(
      Type == 'Golden' ~ Value * 1.5,
      Type == 'Holo' ~ Value *3,
      .default = Value))
  
  return(deck)
}

# --------------------------------------------

gen_special_deck = function(){
  Card_values = sample(seq(7,10,0.01), 5, replace = T)
  C.type.chance = sample(c('Normal','Golden','Holo'), 5, 
                         prob = c(.8,.155,.045), replace = T)
  
  deck = data.frame(Value = Card_values, Type = C.type.chance) %>%
    mutate(Value = case_when(
      Type == 'Golden' ~ Value * 1.5,
      Type == 'Holo' ~ Value *3,
      .default = Value))
  
  return(deck)
}

# -------------------------------------------

calc_cost = function(type, balance){
  price = check_price(type)
  max_amount = floor(balance/price)
  return(max_amount)
}

# -------------------------------------------

buy_deck = function(deck_type, balance){
  # Watch out for typos
  if (deck_type %notin% c('normal','uncommon','special'))
    stop('Type of deck does not exist, you most likely made a typo')
  
  price = check_price(deck_type)
  
  if (deck_type == 'special'){
    if (balance < price){
      deck_type = 'uncommon'
      price = check_price(deck_type)
      message('Not enough money for special deck, buying an uncommon deck instead') 
    }
    else {
      deck = gen_special_deck()
      money_left = balance-price
      return(list(deck = deck, type = deck_type, balance = money_left))
    }
  }

  if (deck_type == 'uncommon'){
    if (balance < price){
      deck_type = 'normal'
      price = check_price(deck_type)
      message('Not enough money for uncommon deck, buying a normal deck') 
    }
    else {
      deck = gen_uncommon_deck()
      money_left = balance-price
      return(list(deck = deck, type = deck_type, balance = money_left))
    }
  }

  if (deck_type == 'normal' & balance >= price){
    deck = gen_normal_deck()
    money_left = balance-price
    return(list(deck = deck, type = deck_type, balance = money_left))
    }
   else{
     message('Not enough money for normal deck either')
     return(NULL)}
}



