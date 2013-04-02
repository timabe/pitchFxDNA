#############################################
#### Pitcher Classification using PitchFx data
#### By Tim Abraham
#### 
#############################################

# url with description of pitchFX pitches:
# http://fastballs.wordpress.com/2007/08/02/glossary-of-the-gameday-pitch-fields/
# github tutorial on pitchFx package http://cpsievert.github.com/pitchRx/demo/
# manual http://cran.r-project.org/web/packages/pitchRx/pitchRx.pdf 

require(ggplot2, plyr, scales)

# load oakland_pitches table
pitch <- read.delim('~/baseball/pitches.txt', header = F, stringsAsFactors = F)
# colnames
nms <- c("ax","ay","az","break_angle","break_length","break_y","des","end_speed",
         "nasty","on_1b","on_2b","on_3b","pfx_x","pfx_z","pitch_type","px","pz",
         "spin_dir","spin_rate","start_speed","sv_id","sz_bot","sz_top","type",
         "type_confidence","vx0","vy0","vz0","x0","y0","z0","zone","url","num",
         "count","pitcher","pitcher_name","batter_name")
# rename df
colnames(pitch) <- nms

# Look at the most frequent pitchers in the data 
freq.pitchers <- data.frame(table(pitch$pitcher_name))
freq.pitchers <- freq.pitchers[order(-freq.pitchers$Freq) , ]

# examine set - pick top 10
# for some reason Tommy Milone gets duplicated as Tom Milone - so remove him
as.character(freq.pitchers$Var1[2:11])->oak.pitchers

pitch <- subset(pitch, pitcher_name %in% oak.pitchers)

# these variables aren't useful for analysis
badVars <- c('des', 'on_1b', 'on_2b', 'on_3b', 'type', 'type_confidence',
                'z0', 'zone', 'url', 'num', 'count', 'pitcher', 'batter_name', 
                'sv_id', 'nasty')

varsToOmit = c(badVars, 'pitcher_name', 'pitch_type')

# let's explore this data . . . use 3 pitchers and see if some dimentions of their fastballs can differentiate them well. 

p3=c('Bartolo Colon', 'Tommy Milone', 'Grant Balfour')
p3.df  <- subset(pitch, pitch_type == 'FF' & pitcher_name %in% p3)

# reduce dimensions of data 
pca <- prcomp(p3.df[ , !colnames(p3.df) %in% varsToOmit], center=T)
# first two principle components explain a high % of the variation in the data
p3.df$PC1 <- pca[[5]][,1]
p3.df$PC2 <- pca[[5]][,2]
ggplot(p3.df, aes(PC1, PC2, color = pitcher_name)) + geom_point(alpha=.8) + scale_color_manual(values = c('darkgreen', 'gold', 'black'))
# one can see 3 clusters for the three pitchers - nice!

# which variables are behind the principle components? 
pca$rotation
# looks like spin_rate and pfx_x
ggplot(p3.df, aes(spin_rate, pfx_x, color = pitcher_name)) +
  geom_point(alpha=.8) + 
  scale_color_manual(values = c('green', 'gold', 'black'))

# how well would this classify with a simple knn algorithm? 
# get some training data 
require(class)
train.vec <- sample(nrow(p3.df), nrow(p3.df) * 0.7)
p3.train <- p3.df[train.vec, ]
p3.test <- p3.df[-train.vec, ]
# train a k nearest neighbor model
testPredictions <- knn(train=p3.train[,c('spin_rate', 'pfx_x')], 
                       test = p3.test[, c('spin_rate', 'pfx_x')], 
                       cl= factor(p3.train[, 'pitcher_name']), k = 1)
# see how the predictions did
table(Real = factor(p3.test$pitcher_name),Predicted = testPredictions)
# nice ! but unfortunately they don't do well on the larger dataset :( 
train.vec2 <- sample(nrow(pitch), nrow(pitch) * 0.7)
pitch.train <- pitch[train.vec2, ]
pitch.test <- pitch[-train.vec2, ]
testPredictions2 <- knn(train=pitch.train[,c('spin_rate', 'pfx_x')], 
                       test = pitch.test[, c('spin_rate', 'pfx_x')], 
                       cl= factor(pitch.train[, 'pitcher_name']), k = 1)
table(factor(pitch.test$pitcher_name), testPredictions2)
# see - not good :( 
# so we need an algorithm that can handle more dimensions and blah blah blah

pitch.train$pitcher_name <- as.factor(pitch.train$pitcher_name)
pitch.train$pitch_type <- as.factor(pitch.train$pitch_type)
pitch.test$pitcher_name <- as.factor(pitch.test$pitcher_name)
pitch.test$pitch_type <- as.factor(pitch.test$pitch_type)
require(randomForest)
rf <- randomForest(factor(pitch.train$pitcher_name) ~ ., 
                   data = pitch.train[ , !colnames(pitch.train) %in% whack.vars], 
                   xtest= pitch.test[ , !colnames(pitch.test) %in% c(whack.vars, 'pitcher_name') ], 
                   ytest=factor(pitch.test$pitcher_name), keep.forest = TRUE)
# Generate Predictions
pitch.test$Prediction <- predict(rf, pitch.test)
TestErrorRate <- as.data.frame(table(pitch.test$pitcher_name, pitch.test$Prediction))
# Calculate prediction accuracy 
TestErrorRate <- ddply(TestErrorRate, .(Var1), transform, Accuracy = Freq/sum(Freq))
# This just takes each players full name and extracts their last name
TestLastNames <- strsplit(as.character(TestErrorRate$Var1), split = '\\s')
TestErrorRate$LastName <- do.call(rbind, TestLastNames)[ , 2]
ggplot(TestErrorRate[TestErrorRate$Var1 == TestErrorRate$Var2, ], aes(LastName, Accuracy)) + 
  geom_bar(fill = 'darkgreen', stat = 'identity') + 
  labs(y = 'Accuracy of Random Forest', x = 'Name') + 
  scale_y_continuous(label = percent)

# how does a individual player do if you aggregate votes across all his games
predictOverFullGames <- function(name) {
  player <- subset(pitch.test, pitcher_name == name)
  # genrate predictions as votes
  player$prediction  <- predict(rf, player, type = 'vote')
  # aggregate over his individual games
  playerAgg <- aggregate(. ~ V1, data = data.frame(cbind( player$url, player$prediction)), sum)
  # melt data frame to extract winner of majority vote
  melt.player <- melt(playerAgg, id = 'V1')
  melt.player$V1 <- as.character(melt.player$V1)
  melt.player <- ddply(melt.player, .(as.character(V1)), summarize, pitcher = variable[value == max(value)])
  return(table(melt.player$pitcher))
}
