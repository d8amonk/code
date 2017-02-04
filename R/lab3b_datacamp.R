# The 'ames' data frame and 'area' and 'price' objects are already loaded
# into the workspace

# The vector 'sample_means50' is initialized with NA values
sample_means50 = rep(NA, 5000)
# The for loop runs 5000 times, with 'i' taking values 1 up to 5000
for (i in 1:5000) {
    # Take a random sample of size 50
    samp = sample(area, 50)
    # Store the mean of the sample in the 'sample_means50' vector on the ith
    # place
    sample_means50[i] = mean(samp)
    # Print the counter 'i'
    print(i)
}

# Print the first few random medians
head(sample_means50)

#COMPARE THE INDUCED VARIATION OF SAMPLE SIZE
# The 'ames' data frame and 'area' and 'price' objects are already loaded
# into the workspace

# Initialize the sample distributions:
sample_means10 = rep(NA, 5000)
sample_means50 = rep(NA, 5000)
sample_means100 = rep(NA, 5000)

# Run the for loop:
for (i in 1:5000) {
    samp = sample(area, 10)
    sample_means10[i] = mean(samp)
    samp = sample(area, 50)
    sample_means50[i] = mean(samp)
    samp = sample(area, 100)
    sample_means100[i] = mean(samp)
}

# Take a look at the results:
head(sample_means10)
head(sample_means50)  # was already loaded
head(sample_means100)

# Divide the plot in 3 rows:
par(mfrow = c(3, 1))

# Define the limits for the x-axis so the scales are the same:
xlimits = range(sample_means10)

# Draw the histograms:
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

#confidence intervals
se = sd(sample(area, 10))/sqrt(10
lower = sample_mean - 1.96 * se #95% CI
upper = sample_mean + 1.96 * se #95% CI
ci.90 <- c(lower, upper)
print(ci.90)

# Initialize 'samp_mean', 'samp_sd' and 'n':
samp_mean = rep(NA, 50)
samp_sd = rep(NA, 50)
n = 60

# For loop goes here:
for (i in 1:50) {
    samp = sample(population, n)
    samp_mean[i] = mean(samp)
    samp_sd[i] = sd(samp)
}

# Calculate the interval bounds here:
lower = samp_mean - 1.96 * (samp_sd/sqrt(n))
upper = samp_mean + 1.96 * (samp_sd/sqrt(n))

# Plotting the confidence intervals:
pop_mean = mean(population)
plot_ci(lower, upper, pop_mean)                 

# Load the 'inference' custom function and run the inference function:
load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/inference.Rdata"))

inference(dataset$variable, type = "ci", method = "simulation", conflevel = 0.9, est = "mean", 
    boot_method = "perc")

#inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, 
#    alternative = "twosided", method = "theoretical")
#this was a hypothesis test (ht,null value)(alternative=less,greater as well)

#You can use the by function as follows to compare the means of the groups: 
by(numerical_dataset, categorical_dataset, mean) 
by(numerical_dataset, categorical_dataset, proportion) 
#You can also use other functions like median, summary, etc. in place of mean
by(numerical_dataset, categorical_dataset, median)
by(nc$birthweight, nc$habit, length) #gives n for number of smokers and non by bw
by(numerical_dataset, categorical_dataset, summary)






