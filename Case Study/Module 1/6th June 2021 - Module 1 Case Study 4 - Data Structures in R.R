#Question1
#a
fruits<- c("Apple","Guava","Banana","Mango")
fruits
class(fruits)

#b
hundred<-c(1:100)
hundred
as.numeric(hundred)->hundred
class(hundred)

#c
logic_game<-c(TRUE,TRUE,FALSE,FALSE)
logic_game
class(logic_game)

#Question2
#a,b,c
jumbo<- list(alphabets=c("A","B","C","D"), numeric=c(55:60),logical=c(TRUE,FALSE))
jumbo

#i)
jumbo$alphabets[3]

#ii)
jumbo$numeric[2]

#iii)
jumbo$logical[1]

#Question3
#a
c1<-c(1,2,3,4)
c2<-c(5,6,7,8)
c3<-c(9,10,11,12)
c4<-c(13,14,15,16)
four_trouble<- matrix(data = c(c1,c2,c3,c4),nrow = 4,ncol = 4,byrow = TRUE)
four_trouble

#Question4
c1<-c(1,2,3,4)
c2<-c(5,6,7,8)
c3<-c(9,10,11,12)
c4<-c(13,14,15,16)
c5<-c(17,18,19,20)
c6<-c(21,22,23,24)
c7<-c(25,26,27,28)
c8<-c(29,30,31,32)
sky_maze<-array(data = c(c1,c2,c3,c4,c5,c6,c7,c8), dim = c(4,4,2))
sky_maze












































