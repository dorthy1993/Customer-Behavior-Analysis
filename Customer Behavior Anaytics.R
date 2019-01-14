data_read<-read.csv(file.choose())

adopt<-filter(data_read,adopter==1)
non_adopter<-filter(data_read,adopter==0)

install.packages("pastecs")
library(pastecs)

# statistic descriptive 

summary_adopt<-stat.desc(adopt)
summary_non_adopt<-stat.desc(non_adopter)

# logistic regression

data_read$subscriber_friend<-ifelse(data_read$subscriber_friend_cnt==0,0,1)

mylogit<-glm(adopter~ friend_cnt + avg_friend_age + avg_friend_male + subscriber_friend + friend_country_cnt+ songsListened+lovedTracks+posts+playlists+shouts,data=data_read,family=binomial())

summary(mylogit)
exp(coef(mylogit))



prs_df <- data.frame(pr_score = predict(mylogit, type = "response"),
                     adopter=mylogit$model$adopter)
head(prs_df)
head(mylogit_reduce$model)

#plot

labs <- paste(":", c("subscriber-friend", "no-subscreiber-friend"))

prs_df %>%
  mutate(adopter = ifelse(adopter == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~adopter) +
  xlab("Probability of being an adopter") +
  theme_bw()

#PSM 

attach(data_read)

data_read$subscriber_friend<-ifelse(data_read$subscriber_friend_cnt==0,0,1)

t.test(data_read$adopter~data_read$subscriber_friend)

install.packages("MatchIt")
library(MatchIt)

library(dplyr)

adopter_cov <- c('age', 'male', 'friend_cnt', 'avg_friend_age','avg_friend_male', 'friend_country_cnt',"subscriber_friend_cnt","songsListened",'lovedTracks',"posts",'playlists',"shouts",'tenure','good_country')
lapply(adopter_cov, function(v) {
  t.test(data_read[, v] ~ data_read$adopter)
})

adopter_nomiss <- data_read %>%  # MatchIt does not allow missing values
  select(adopter, one_of(adopter_cov)) %>%
  na.omit()


mod_match_adopt <- matchit(subscriber_friend ~ age + male + friend_cnt + avg_friend_age + avg_friend_male 
                           + friend_country_cnt+ songsListened+lovedTracks+posts+playlists+shouts+tenure+good_country
                           , method = "nearest", data = data_read)

summary(mod_match_adopt)
plot(mod_match_adopt)

dta_m_adopt <- match.data(mod_match_adopt)

dim(dta_m_adopt)

#Visual Inspection

fn_bal <- function(dta_m_adopt, variable) {
  dta_m_adopt$variable <- dta_m_adopt[, variable]
  dta_m_adopt$subscriber_friend <- as.factor(dta_m_adopt$subscriber_friend)
  support <- c(min(dta_m_adopt$variable), max(dta_m_adopt$variable))
  ggplot(dta_m_adopt, aes(x = distance, y = variable, color = subscriber_friend)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)
grid.arrange(
  fn_bal(dta_m_adopt, "age"),
  fn_bal(dta_m_adopt, "male") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "friend_cnt"),
  fn_bal(dta_m_adopt, "avg_friend_age") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "avg_friend_male"),
  fn_bal(dta_m_adopt, "friend_country_cnt") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "songsListened"),
  fn_bal(dta_m_adopt, "lovedTracks") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "posts"),
  fn_bal(dta_m_adopt, "playlists") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "shouts"),
  fn_bal(dta_m_adopt, "tenure") + theme(legend.position = "none"),
  fn_bal(dta_m_adopt, "good_country"),
  nrow = 7, widths = c(1, 0.8)
)

#Difference in means

with(dta_m_adopt, t.test(adopter~subscriber_friend))

lm_treat1_adopter <- glm(adopter~subscriber_friend, data = dta_m_adopt,family=binomial())
summary(lm_treat1_adopter)
exp(coef(lm_treat1_adopter))

lm_treat2_adopter <- glm(adopter~age + male + friend_cnt + avg_friend_age + avg_friend_male + friend_country_cnt+ subscriber_friend+ songsListened+lovedTracks+posts+playlists+shouts+tenure+good_country, data = dta_m_adopt,family = binomial())
summary(lm_treat2_adopter)
exp(coef(lm_treat2_adopter))