# Outift generator by Pierre Hardy

# packages

library(readxl)
library(dplyr)
library(owmr)

# Load dataset of outfit matchings

outfits <- read_excel("Dataset for Outfit Generator.xlsx", sheet=1)

# Load dataset of wardrobe

wardrobe <- read_excel("Dataset for Outfit Generator.xlsx", sheet=2)

# set up weather info

Sys.setenv(OWM_API_KEY = "b9c56fccedc73472a1dfeb5c86a5d2d3")

# Create function for heat assignment

heat.ass <- function(temp){
  if (temp>=20){
    heat <- 1
  } else if (temp >= 15){
    heat <- 2
  } else if (temp >= 10){
    heat <- 3
  } else if (temp >=5){
    heat <-4
  } else if (temp >= 0){
    heat <- 5
  } else {
    heat <-6
  }
  return(heat)
}

# picker for errands
pickerrands <- function(article, heat){
  # set.seed(as.integer(Sys.Date()))
  if (heat >= 2){
    shortlist <- select(wardrobe, Articles, "Errand")
  } else {
    shortlist <- select(wardrobe, Articles, "Errand Cold")
  }
  shortlist <- filter(shortlist, wardrobe$Articles==article)
  pick <- shortlist[2]
  return(pick)
}

# function for errands
errand <- function(heat){
  shortlist <- filter(outfits, outfits$Outerwear=="Coat")
  shortlist <- filter(shortlist, shortlist$Suit!="Suit")
  shortlist <- filter(shortlist, shortlist$Jacket!="Blazer" | shortlist$Knit!="Jacket")
  shortlist <- filter(outfits, outfits$Heat==heat)
  ootd <- sample_n(shortlist, 1)
  ootd <- select(ootd, Outerwear, Suit, Jacket, Knit, Top, Bottom, Shoes)
  ootd <- as.character(ootd)
  ootd <- na.omit(ootd)
  ootd <- as.character(ootd)
  n.items <- length(ootd)
  ootd <- sapply(ootd, pickerrands, heat=heat)
  if (heat==4){
    ootd <- c(ootd, wardrobe[16,10])
  } else if (heat==5){
    ootd <- c(ootd, wardrobe[16,12])
  } else if (heat ==6) {
    ootd <- c(ootd, wardrobe[16,14])
  }
  return(ootd)
  
}

# function for runs
run <- function(heat){
  if (heat <= 1){
    shortlist <- select(wardrobe, Run)
    pick <- na.omit(shortlist)
    return(pick)                    
  } else if (heat <= 3) {
    shortlist <- select(wardrobe, "Cold Run 1")
    pick <- na.omit(shortlist)
    return(pick) 
  } else if (heat <= 4) {
    shortlist <- select(wardrobe, "Cold Run 2")
    pick <- na.omit(shortlist)
    return(pick)  
  } else if (heat <= 5) {
    shortlist <- select(wardrobe, "Cold Run 3")
    pick <- na.omit(shortlist)
    return(pick) 
  } else if (heat <= 6) {
    shortlist <- select(wardrobe, "Cold Run 4")
    pick <- na.omit(shortlist)
    return(pick)
  }
}

# function for at home
home <- function(heat){
  if (heat <= 1){
    shortlist <- select(wardrobe, Home)
    pick <- na.omit(shortlist)
    return(pick)                    
  } else if (heat <= 3) {
    shortlist <- select(wardrobe, "Home Cold 1")
    pick <- na.omit(shortlist)
    return(pick) 
  } else if (heat <= 4) {
    shortlist <- select(wardrobe, "Home Cold 2")
    pick <- na.omit(shortlist)
    return(pick)  
  } else if (heat <= 5) {
    shortlist <- select(wardrobe, "Home Cold 3")
    pick <- na.omit(shortlist)
    return(pick) 
  }
}

# Create function for clothes picker

picker <- function(article, precip){
  # set.seed(as.integer(Sys.Date()))
  if (precip=="Thunderstorm" | precip=="Drizzle" | precip=="Rain" | precip=="Snow"){
    shortlist <- select(wardrobe, Articles, Rain)
  } else {
    shortlist <- select(wardrobe, Articles, Capsule)
  }
  shortlist <- filter(wardrobe, wardrobe$Articles==article)
  pick <- shortlist[2]
  return(pick)
}

# Create function for curator based on temp
curator <- function(heat, precip){
  ootd <- filter(outfits, outfits$Heat==heat)
  ootd <- sample_n(ootd, 1)
  tips <- select(ootd, Accessory, Button, Socks, Tuck, Belt)
  tips <- tips[,complete.cases(t(tips))]
  ootd <- select(ootd, Outerwear, Suit, Jacket, Knit, Top, Bottom, Shoes)
  ootd <- as.character(ootd)
  ootd <- na.omit(ootd)
  ootd <- as.character(ootd)
  n.items <- length(ootd)
  ootd <- sapply(ootd, picker, precip=precip )
  if (heat==4){
    ootd <- c(ootd, wardrobe[16,10])
  } else if (heat==5){
    ootd <- c(ootd, wardrobe[16,12])
  } else if (heat ==6) {
    ootd <- c(ootd, wardrobe[16,14])
  }
  ootd <- c(ootd,tips)
  return(ootd)
}

# Create function for random generation

outfit.maker <- function(temp.low, temp.high, precip, purpose){
  # set.seed(as.integer(Sys.Date()))
  heat.low <- heat.ass(temp.low)
  heat.high <- heat.ass(temp.high)
  if (heat.high-heat.low>1){
    heat <- heat.high - 1
  } else {
    heat <- heat.low
  }
  
  if (purpose == 1){
    ootd <- curator(heat, precip)
  } else if (purpose == 2) {
    ootd <- errand(heat)
  } else if (purpose == 3){
    ootd <- run(heat)
  } else if (purpose == 4){
    ootd <- home(heat)
  } else {
    stop("Follow the fucking instructions.")
  }
  
  print(ootd)
  print(paste("Weather: ", precip))
  print(paste("Low: ", temp.low))
  print(paste("High: ", temp.high))
}

# Ask for input

loc <- readline(prompt= "Where are you? ")
purpose <- readline(prompt="What are you dressing for? 1 - going out; 2 - errands; 3 - running; 4 - stay at home ")
temp.low <- get_current(loc, units="metric")$main$temp_min
temp.high <- get_current(loc, units="metric")$main$temp_max
precip <- get_current(loc, units="metric")$weather[1,2]
outfit.maker(temp.low, temp.high, precip, purpose)