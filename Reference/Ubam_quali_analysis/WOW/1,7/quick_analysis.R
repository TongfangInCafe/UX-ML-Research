library(ggplot2)


## race 1 career 7 quick analysis
dat_2 <- read.csv("C:/Users/z003nefs/Desktop/1,7/2.txt", header=FALSE)
dat_4 <- read.csv("C:/Users/z003nefs/Desktop/1,7/4.txt", header=FALSE)

dat_25 <- read.csv("C:/Users/z003nefs/Desktop/1,7/25.txt", header=FALSE)

dat_31 <- read.csv("C:/Users/z003nefs/Desktop/1,7/31.txt", header=FALSE)


dat_36 <- read.csv("C:/Users/z003nefs/Desktop/1,7/36.txt", header=FALSE)



dat_41 <- read.csv("C:/Users/z003nefs/Desktop/1,7/41.txt", header=FALSE)


# 2 
length <- length(dat_25$V1)
time <- c(1:length)

dat25 <- data.frame(dat_25, time)
write.table(dat25, "C:/Users/z003nefs/Desktop/mydata25.txt", sep="\t")






length <- length(dat_36$V1)
time <- c(1:length)

dat36 <- data.frame(dat_36, time)
write.table(dat36, "C:/Users/z003nefs/Desktop/mydata36.txt", sep="\t")



ZONE <- dat_2$V8
TIME <- c(1:length(timelength))
LEVEL <- as.factor(dat_2$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")



# 4
timelength <- as.numeric(length(dat_4$V1))

ZONE <- dat_4$V8
TIME <- c(1:timelength)
LEVEL <- as.factor(dat_4$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")



# 25
timelength <- as.numeric(length(dat_25$V1))

ZONE <- dat_25$V8
TIME <- c(1:timelength)
LEVEL <- as.factor(dat_25$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")




# 31
timelength <- as.numeric(length(dat_31$V1))

ZONE <- dat_31$V8
TIME <- c(1:timelength)
LEVEL <- as.factor(dat_31$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")




# 36
timelength <- as.numeric(length(dat_36$V1))

ZONE <- dat_36$V8
TIME <- c(1:timelength)
LEVEL <- as.factor(dat_36$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")



# 41
timelength <- as.numeric(length(dat_41$V1))

ZONE <- dat_41$V8
TIME <- c(1:timelength)
LEVEL <- as.factor(dat_41$V5)

df <- data.frame(TIME, ZONE, LEVEL)

plot(TIME, ZONE, xlab = "Time Period", ylab = "Zone Change", type = "l")

ggplot(df, aes(x = df$TIME, y = df$ZONE)) + 
  geom_point(aes(colour = factor(df$LEVEL))) +
  xlab("Time Period") +
  ylab("Zone Change")
