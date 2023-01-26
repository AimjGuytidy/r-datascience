library(stringr)


# A####
# the key differences between class() and typeof()

# class():prints the vector of names of classes an object inherits from.The class function in R helps us to understand the type of object
# typeof(): determines the (R internal) type or storage mode of any object


#B####

gas_price<-'1609' 
address1<-'1 KG6 AVE' 
address2<-'20 KK 785 ST' 

# a conditional statement that prints out values based on desired conditions
#in our case we are printing gas_price as numeric 

if (is.numeric(gas_price)){
  print(gas_price)
}else{
  print(as.numeric(gas_price))
}

# a conditional statement that checks whether the variable address1 is equal,
#shorter or longer than address2 and prints out accordingly

if (setequal(nchar(address1),nchar(address2))){
  print("address1 and address2 have the same length")
  
}else if (nchar(address1)>nchar(address2)){
  print("address1 is longer than address2")
}else{
  print("address1 is shorted")
}

#C####

dist_vec <- c(
  "Bugesera", "Gatsibo", "Kayonza", "Kirehe", "Ngoma", "Nyagatare", "Rwamagana",
  "Gasabo", "Kicukiro", "Nyarugenge", "Burera", "Gakenke", "Gicumbi", "Musanze",
  "Rulindo", "Gisagara", "Huye", "Kamonyi", "Muhanga", "Nyamagabe", "Nyanza",
  "Nyaruguru", "Ruhango", "Karongi", "Ngororero", "Nyabihu", "Nyamasheke",
  "Rubavu", "Rusizi", "Rutsiro")
#printing districts that start with R or r
str_subset(dist_vec,"(?i)^r") 

#printing longest districts in terms of number of characters
for (i in dist_vec){
  if (nchar(i) == max(nchar(dist_vec))){
    print(i)
  }
}


#D####

names<-list('Francine', 'JB','Sankara','Elise') 
tel<-list('0783890690','0780745992','0787556224','0789271106') 

# combining 2 lists
combined_vec <- cbind(names,tel)
combined_vec
df_combined <- as.data.frame(combined_vec)# creating the dataframe of combined vectors
view(df_combined)
#checking the dimension of the dataframe
str(df_combined) # from the structure of the dataframe, it is a 4 x 2 dataframe
dim(df_combined) # this is a direct way of determining the dimension of our dataframe
