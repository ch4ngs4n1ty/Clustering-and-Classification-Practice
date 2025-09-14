#############
#Part A - Experimenting with Gaussian Random Numbers
#############

N <- 10000 #10,000 Gaussian Random Values
mu <- 0 #They all start with mu of 0
sigma <- 1.0 #Standard Deviation of 1.0

X <- rnorm(N, mu, sigma) #First Vector
Y <- rnorm(N, mu, sigma) #Second Vector
Z <- rnorm(N, mu, sigma) #Third Vector
S <- rnorm(N, mu, sigma) #Fourth Vector
T <- rnorm(N, mu, sigma) #Fifth Vector
U <- rnorm(N, mu, sigma) #Sixth Vector

distA <- sqrt(X^2) #Distance in 1D: dist = sqrt(X^2)
fracA <- mean(distA <= 1.0)

distB <- sqrt(X^2 + Y^2) #Distance in 2D: dist = sqrt(X^2 + Y^2)
fracB <- mean(distB <= 1.0)

distC <- sqrt(X^2 + Y^2 + Z^2) #Distance in 3D: dist = sqrt(X^2 + Y^2 + Z^2)
fracC <- mean(distC <= 1.0)

disigma <- sqrt(X^2 + Y^2 + Z^2 + S^2) #Distance in 4D: dist = sqrt(X^2 + Y^2 + Z^2 + S^2)
fracD <- mean(disigma <= 1.0)

distE <- sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2) #Distance in 5D: dist = sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2)
fracE <- mean(distE <= 1.0)

distF <- sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2 + U^2) #Distance in 6D: dist = sqrt(X^2 + Y^2 + Z^2 + S^2 + T^2 + U^2)
fracF <- mean(distF <= 1.0)

dims <- 1:6 #Dimensions from 1 to 6 X-Axis
fracs <- c(fracA, fracB, fracC, fracD, fracE, fracF) #Data (Fraction of data in 1 std of origin) For Y-Axis 

png("partA_plot.png", width = 1200, height = 600) #Generates png file of Part A graph   

plot(dims, fracs,
main = "Gaussian Random Numbers in N Dimensions", #Title on top of the graph
type = "o", #Displays points and lines in graph
pch = 1, #Plotting character, representing symbols. Ex: 1 = circle, (There are 0 to 25 options)
lty = 2, #Line type like blank, solid, dashed, dotted etc
ylim = c(0,1),  #Y-Axis limits
xlim = c(0,7),  #X-Axis limits
xlab = "Number of elements (dimension)",
ylab = "Fraction within 1 sigma dev",
xaxs = "i", #To make X-Axis start as 0
yaxs = "i" #To make Y-Axis start as 0
)

dev.off() #Purpose is to tell script to stop writing file and close it

#Graph shows that as dimension increase, the fraction of points withing 1 sigma decreases.

#############
#Part B - Clustering By Computing Otsu's Method
#############

#TRAFFIC_STATIONS_2251 -> TrafficStation_2251_0# -> Data_0#.csv

main_folder <- "TRAFFIC_STATIONS_2251"

csvs <- list.files(main_folder, pattern = "Data_.*\\.csv", recursive = TRUE, full.names = TRUE)

dfs <- lapply(csvs, read.csv)

data_all <- do.call(rbind, dfs) #Combine all data frames into one data frame

all_speeds <- round(data_all$SPEED) #Quantitize speed data to nearest one mile per hour

threshold <- 45:85 #Thresholds from 45 to 85 miles per hour

mixed_variance <- numeric(length(threshold)) #Purpose of this is to create mixed variance vectors of all 45 - 85 thresholds

for (i in seq_along(threshold)) {

    t <- threshold[i] #Current threshold

    left_set <- all_speeds[all_speeds <= t] #Vectors of left set
    right_set <- all_speeds[all_speeds > t] #Vectors of right set

    W_L <- length(left_set) / length(all_speeds)  #Formula: W_L = abs(left_set) / abs(speeds)
    W_R <- 1 - W_L #Remainder of left set. Formula: W_R = 1 - W_L

    if (length(left_set) >= 2) { #sigma_L2 = var(left_set)
        sigma_L2 <- var(left_set)
    } else {
        sigma_L2 <- 0
    }

    if (length(right_set) >= 2) { #sigma_R2 = var(right_set)
        sigma_R2 <- var(right_set)
    } else {
        sigma_R2 <- 0
    }
    mv_nround <- W_L * sigma_L2 + W_R * sigma_R2 #Formula: mixed_variance = W_L * sigma_L2 + W_R * sigma_R2
    mixed_variance[i] = round(mv_nround * 10) / 10 #Round to nearest tenth and remove noise
}

png("partB_plot.png", width = 1200, height = 600)  # width > height makes it wide

x <- threshold

par(mfrow=c(1,1)) #1 row 1 column

plot(x, mixed_variance,
     main = "Mixed Variance vs Threshold",
     type = "l",
     col = "blue",
     pch = 1,
     lty = 1,
     lwd = 2,
     xlab = "Speed",
     ylab = "Mixed Variance",
     ylim = c(0, 20),
     xaxs = "i", #To make X-Axis start as 0
     yaxs = "i", #To make Y-Axis start as 0
)
axis(1, at = x, labels = x)
axis(2)

# v = x (Places vertical grid lines with x values)
abline(v = x, col = "lightgrey", lty = 1, lwd = 2)   # vertical grid
abline(h = axTicks(2), col = "lightgrey", lty = 1, lwd = 2)  # horizontal grid

dev.off()

#############
#Part C - Computing Fraction of False Alarms, Fraction of Misses, and Fraction of Total Mistakes
#############

intent <- data_all$INTENT

nonagg_index = intent %in% c(0, 1) #Non Aggressive Index
agg_index = intent == 2 #Aggressive Index

speeds_nonagg <- all_speeds[nonagg_index]
speeds_agg <- all_speeds[agg_index]

false_alarms <- numeric(length(threshold)) #False Alarms
missed_detections <- numeric(length(threshold)) #Missed Detections
total_mistakes <- numeric(length(threshold)) #False Negatives

for (i in seq_along(threshold)) {

    t <- threshold[i] #Current threshold

    a <- mean(speeds_nonagg > t)
    b <- mean(speeds_agg <= t)
    mistakes <- a + b

    false_alarms[i] <- a
    missed_detections[i] <- b
    total_mistakes[i] <- mistakes

}

png("partC_plot.png", width = 1200, height = 600)  # width > height makes it wide

x <- threshold
ylim_range <- c(0, 1)

plot(x, false_alarms,
    type = "o",
    col = "red",
    lwd = 2, 
    lty = 2,
    pch = 15,
    ylim = c(0, 1),
    xlab = "Threshold Speed",
    ylab = "Fraction",
    main = "False Alarms and Missed Detections vs Threshold Speed",
    xaxt = "n",
    xaxs = "i", #To make X-Axis start as 0
    yaxs = "i" #To make Y-Axis start as 0
)

axis(1, at = x, labels = x)
lines(x, missed_detections, type="o", col="blue", pch=2, lwd=2)     # triangles
lines(x, total_mistakes,    type="o", col="magenta", pch=1, lwd=2)  # circles

legend("topright", legend = c("False Alarms", "Missed Detections", "Total Mistakes"),
       col = c("red", "blue", "magenta"),
       pch = c(15, 2, 1),
       lty = c(2, 1, 1),
       lwd = 2
)

dev.off()

#############
#Write Up For Question 2
#############

#According to research with table() function, it can automatically sort them in ascending numeric order or alphabetical order so everything in graph looks organized.
tab <- table(intent, all_speeds) 

png("writeup2_barplot.png", width = 1200, height = 600)  # width > height makes it wide

barplot(tab, 
        beside = TRUE,
        col = c("lightgrey", "darkgrey", "black"),
        xlab = "Speed (mph)",
        ylab = "Count",
        main = "Histogram of Vehicle Speeds by Driver Intent",
        xaxs = "i" #To make X-Axis start as 0
)

legend("topright", legend = c("0", "1", "2"), fill = c("lightgrey", "darkgrey", "black"))

dev.off()