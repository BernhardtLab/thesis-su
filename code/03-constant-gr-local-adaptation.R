#local adaptation
output_norberg <- read.csv("data/output-norberg.csv")
View(output_norberg)
str(output_norberg)

output_norberg2 <- output_norberg %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_")
View(output_norberg2)

output_norberg2 %>% 
  rename("Treatment" = "incubator") %>% 
  ggplot(aes(temp, predicted_growth, colour = Treatment)) + 
  geom_line() + 
  ylim(0, 1.5) + 
  theme_minimal() + 
  xlab("Temperature") + 
  ylab("Predicted growth") + 
  theme(
    panel.grid = element_blank(),  # Removes all grid lines
    strip.text = element_text(face = "bold")  # Makes facet labels (temp) bold
  )
# preds constants at 14 and 30 deg ----------------------------------------------
###14 deg
t14C_14deg <- output_norberg2 %>%
  filter(incubator == "14C", temp == 14.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t14C_14deg$predicted_growth) #W = 0.95337, p-value = 0.7451

t30C_14deg <- output_norberg2 %>%
  filter(incubator == "30C", temp == 14.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t30C_14deg$predicted_growth) #W = 0.97458, p-value = 0.9313
#normal data --> t test

t.test(t14C_14deg$predicted_growth, t30C_14deg$predicted_growth) #p-value = 3.151e-10
t.test(t14C_14deg$predicted_growth, t30C_14deg$predicted_growth, alternative = "greater") #p-value = 1.575e-10
#preds: 14C is sig greater than 30C at 14 deg

###30 deg
t14C_30deg <- output_norberg2 %>%
  filter(incubator == "14C", temp == 30.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t14C_30deg$predicted_growth) #W = 0.82771, p-value = 0.05619

t30C_30deg <- output_norberg2 %>%
  filter(incubator == "30C", temp == 30.0) %>%
  select(incubator, flask, predicted_growth, topt, rmax, tmax)
shapiro.test(t30C_30deg$predicted_growth) #W = 0.97289, p-value = 0.9196
#normal data --> t test

t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth) #p-value = 1
t.test(t14C_30deg$predicted_growth, t30C_30deg$predicted_growth, alternative = "greater") #p-value = 0.5
#preds: no sig diff between 14C and 30C at 30 deg

# actual constants at 14 deg and 30 deg -----------------------------------
actual <- read.csv("data/growthtool-gdat-sum.csv")

actual <- actual %>% 
  separate(col = treatment, into = c("incubator", "flask"), sep = "_") %>% 
  separate(col = unique_well, into = c("plate", "well", "tpctemp")) %>% 
  unite("unique_well", "plate", "well", sep = "_") %>% 
  rename("temp" = "tpctemp")

###14 deg
act_t14C_14deg <- actual %>%
  filter(incubator == "14C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t14C_14deg$mu <- as.numeric(act_t14C_14deg$mu)
shapiro.test(act_t14C_14deg$mu) #W = 0.96331, p-value = 0.5083

act_t30C_14deg <- actual %>%
  filter(incubator == "30C", temp == 14.0) %>%
  select(incubator, flask, mu)
act_t30C_14deg$mu <- as.numeric(act_t30C_14deg$mu)
shapiro.test(act_t30C_14deg$mu) #W = 0.93776, p-value = 0.1454

t.test(act_t14C_14deg$mu, act_t30C_14deg$mu) #p-value = 2.12e-12
t.test(act_t14C_14deg$mu, act_t30C_14deg$mu, alternative = "greater") #p-value = 1.06e-12
#act: 14C is sig greater than 30C at 14 deg

###30 deg
act_t14C_30deg <- actual %>%
  filter(incubator == "14C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t14C_30deg$mu <- as.numeric(act_t14C_30deg$mu)
shapiro.test(act_t14C_30deg$mu) #W = 0.96316, p-value = 0.505

act_t30C_30deg <- actual %>%
  filter(incubator == "30C", temp == 30.0) %>%
  select(incubator, flask, mu)
act_t30C_30deg$mu <- as.numeric(act_t30C_30deg$mu)
shapiro.test(act_t30C_30deg$mu) #W = 0.8792, p-value = 0.00801
#not normally distributed

#Wilcox
wilcox.test(act_t14C_30deg$mu, act_t30C_30deg$mu) #W = 507, p-value = 1.295e-06
#30 different

