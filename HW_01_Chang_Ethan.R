#Part A

N <- 10000 #10,000 Gaussian Random Values
mu <- 0 #They all start with mu of 0
sigma <- 1.0 #Standard Deviation of 1.0

X <- rnorm(N, mu, sigma) #First Vector
Y <- rnorm(N, mu, sigma) #Second Vector
Z <- rnorm(N, mu, sigma) #Third Vector
S <- rnorm(N, mu, sigma) #Fourth Vector
T <- rnorm(N, mu, sigma) #Fifth Vector
U <- rnorm(N, mu, sigma) #Sixth Vector

distA <- sqrt(X^2)
fracA <- mean(distA <= 1.0)
print(fracA)

distB <- sqrt(X^2 + Y^2)
fracB <- mean(distB <= 1.0)
print(fracB)

distC <- sqrt(X^2 + Y^2 + Z^2)
fracC <- mean(distC <= 1.0)
print(fracC)

disigma <- sqrt(X^2 + Y^2 + Z^2 + S^2)
fracD <- mean(disigma <= 1.0)
print(fracD)

distE <- sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2)
fracE <- mean(distE <= 1.0)
print(fracE)

distF <- sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2 + U^2)
fracF <- mean(distF <= 1.0)
print(fracF)

dims <- 1:6
fracs <- c(fracA, fracB, fracC, fracD, fracE, fracF)

#print(data.frame(dimension = dims, fraction = fracs))

plot(dims, fracs,
main = "Gaussian Vectors in N Dimensions",
type = "o",
pch = 1,
lty = 2,
ylim = c(0,1), # y axis between 0 and 1 (fractions)
xlab = "Number of elements (dimension)",
ylab = "Fraction within 1 sigma dev"
)

#Graph shows that as dimension increase, the fraction of points withing 1 sigma decreases.

#############################
#Part B 
#############################

#TRAFFIC_STATIONS_2251 -> TrafficStation_2251_0# -> Data_0#.csv

main_folder <- "/Users/ch4ngs4n1ty/CSCI 420/HW 1/TRAFFIC_STATIONS_2251"

idx <- sprintf("%02d", 1:32)   # "01", "02", ..., "20"

subs <- file.path(main_folder, paste0("TrafficStation_2251_", idx))

csvs <- file.path(subs, paste0("Data_", idx, ".csv"))

dfs <- lapply(csvs, read.csv)

data_all <- do.call(rbind, dfs)

# show the column names
names(data_all)

# see first few rows
head(data_all)

# show structure (types of each column)
str(data_all)

speeds <- round(data_all$SPEED) #Quantitize speed data to nearest one mile per hour
#intents <- data_all$INTENT

threshold <- 45:85 #Thresholds from 45 to 85 miles per hour

mixed_variance <- numeric(length(threshold))

#print(mixed_variance)

#Now we got mixed variance of two sets versus threshold

#We will now start Otsu's method