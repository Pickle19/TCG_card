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
# This function will be used to generate the packs. It takes wanted type of deck
# and all the different value ranges and probability chances may be used to simulate 
# different situations

gen_deck = function(Type, Val_norm = Val_norm, Prob_norm = Prob_norm,
                    Val_unc = Val_unc, Prob_unc = Prob_unc,
                    Val_spec = Val_spec, Prob_spec = Prob_spec){
  
  Card_types = c('Normal','Golden','Holo')
  
  if (Type == 'normal'){
    Card_values = sample(Val_norm, 10, replace = T)
    C_type_chance = sample(Card_types, 10, prob = Prob_norm, replace = T)
  }
  if (Type == 'uncommon'){
    Card_values = sample(Val_unc, 10, replace = T)
    C_type_chance = sample(Card_types, 10, prob = Prob_unc, replace = T)
  }
  if (Type == 'special'){
    Card_values = sample(Val_spec, 5, replace = T)
    C_type_chance = sample(Card_types, 5, prob = Prob_spec, replace = T)
  }
  
  deck = data.frame(Value = Card_values, Type = C_type_chance) %>%
    mutate(Value = case_when(
      Type == 'Golden' ~ Value * 1.5,
      Type == 'Holo' ~ Value *3,
      .default = Value))
  
  return(deck)
}

# --------------------------------------------

calc_cost = function(type, balance){
  price = check_price(type)
  max_amount = floor(balance/price)
  return(max_amount)
}


# ------------------------------------------
# This will be the function which allows us to buy the decks. It takes the same inputs
# as the one which generates them, with the addition of balance. By comparing the price to our
# balance we can check if we can afford the respective deck or not. If we can't buy it, it returns
# NULL - this part is important


buy_deck = function(deck_type, balance,
                    Val_norm = Val_norm, Prob_norm = Prob_norm,
                    Val_unc = Val_unc, Prob_unc = Prob_unc,
                    Val_spec = Val_spec, Prob_spec = Prob_spec){
  # Check for  typeos
  if (deck_type %notin% c('normal','uncommon','special'))
    stop('Type of deck does not exist, you most likely made a typo')
  
  price = check_price(deck_type)
  
  if (balance < price){
    message("You dont have enough money, let's try an uncommon deck instead")
    deck_type = 'uncommon'
    price = check_price(deck_type)
    
    if(balance < price){
      message('You still dont have enough money, probably gonna be enough for a normal deck')
      deck_type = 'normal'
      price = check_price(deck_type)
    }
      if (balance < price){ # If we don't the money needed, we get NULL
        message("You're too poor to buy even a normal deck")
        return(NULL)
      }
  }
  
  money_left = balance - price # calculating our money left after the transaction
  deck = gen_deck(deck_type,Val_norm = Val_norm, Prob_norm = Prob_norm,
                            Val_unc = Val_unc, Prob_unc = Prob_unc,
                            Val_spec = Val_spec, Prob_spec = Prob_spec)
  
  # It return a list with all the variables needed, to assemble our result data frame 
  # in the simulation
  
  return(list(deck = deck, type = deck_type, balance = money_left))
}

