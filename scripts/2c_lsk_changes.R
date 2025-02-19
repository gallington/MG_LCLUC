# calc lsk totals per hh and changes past 5 yrs

#Livestock species numbers we converted to sheep forage units (SFU) 
# using conversion factors of 
# 1 = sheep
# 0.9 = goats
# 6 = cattle 
# 7 = horses and 
# 5 = camels 
#(Bedunah and Schmidt, 2000)

# calculate SFU per hh:

sfu23 <- base_lsk %>% group_by(Ref) %>%
  summarise(sfu23 = (((camel23 * 5) +
                    (cow23 * 6) +
                    (horse23 * 7) +
                    (sheep23 * 1) +
                    (goat23 * 0.9))
                    )) 

sfu <- base_lsk %>% 
  mutate(sfu23 = (((camel23 * 5) +
                        (cow23 * 6) +
                        (horse23 * 7) +
                        (sheep23 * 1) +
                        (goat23 * 0.9))
  )) %>%
  mutate(sfu5Y =  (((camel5Y * 5) +
                    (cow5Y * 6) +
                    (horse5Y * 7) +
                    (sheep5Y * 1) +
                    (goat5Y * 0.9))
                    )) %>%
  mutate(sfuChange = (sfu23 - sfu5Y))
           

ggplot(sfu, aes(x= sfu23)) +
  geom_density()+
  geom_density(aes(x=sfu5Y), color = 'blue')



ggplot(sfu, aes(x= sfuChange)) +
  geom_density()

# look for outliers
sfu %>% summarise(max(sfu23, na.rm = TRUE))
# count how many unrealistically huge herds
sfu %>% filter(sfu23 > 2000)%>% summarise(n=n())

sfu %>% filter(sfuChange < -2000)%>% summarise(n=n())


boxplot(sfu$camel23, 
        sfu$cow23,
        sfu$horse23,
       # sfu$sheep23,
       # sfu$goat23,
        data = sfu)

boxplot(sfu$sheep23,
        sfu$goat23,
        data = sfu)

boxplot(sfu$camel5Y, 
        sfu$cow5Y,
        sfu$horse5Y,
        data = sfu)

boxplot(sfu$sheep5Y,
        sfu$goat5Y,
        data = sfu)

plot(sfu$sheep5Y, sfu$sheep23, xlim= c(0,2000), ylim = c(0,2000)) 
abline(0,1)

plot(sfu$camel5Y, sfu$camel23, xlim= c(0,60), ylim = c(0,60))  # people are mostly reducing number of camels?
abline(0,1)

plot(sfu$horse5Y, sfu$horse23, xlim = c(0,500)) 
abline(0,1)

plot(sfu$cow5Y, sfu$cow23, ylim = c(0,200)) 
abline(0,1)

plot(sfu$goat5Y, sfu$goat23, xlim= c(0,1000), ylim = c(0,1000))
abline(0,1)

plot(sfu$sfu5, sfu$sfu23, xlim= c(0,9000), ylim = c(0,9000))
abline(0,1)
# sfu %>% pivot_longer(
#   cols = camel23:goat5Y,
#   names_to = c("Type", "Time"),
#   names_pattern = 
#   values_to = c("Total"),
#   values_drop_na = TRUE
# )

