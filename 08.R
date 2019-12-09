library(magrittr)
SIF <- scan("08_input.txt", what = "character")
SIF <- stringr::str_split(SIF, "")
SIF <- unlist(SIF)
SIF <- as.integer(SIF)
SIF <- array(SIF, dim = c(25,6, 100))
SIF <- aperm(SIF, c(2,1,3))


fewest_zeroes <- apply(SIF, 3, function(x){sum(x == 0)}) %>% 
  which.min()

part1 <- sum(SIF[,,fewest_zeroes]  == 1) * sum(SIF[,,fewest_zeroes]  == 2) 
part1
 


message <- apply(SIF, c(1,2) ,function(x){x[x !=2][1]} )

cbind(expand.grid( y = -(1:6), x = 1:25), value = as.vector(message)) %>% 
  ggplot()+
  geom_tile(aes(x, y, fill = value)) +
  coord_fixed()




 
 
 
      