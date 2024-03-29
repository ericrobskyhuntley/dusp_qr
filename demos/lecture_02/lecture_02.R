# The provided data is 5-year ACS estimates of renter- and owner-occupied housing units
# per census tract in 2021. I created this file using an R script! For the curious, it's
# here: https://github.com/ericrobskyhuntley/dusp_qr/blob/main/demos/lecture_02_data.R

# Barplots

tracts_by_county <- read.csv('l02_demo_grouped.csv')

barplot(tracts_by_county$maj_renter_count, names.arg = tracts_by_county$county, las=2)

# Histograms

df <- read.csv('l02_demo.csv')

hist(df$pct_rent_occ, freq = TRUE)
hist(df$pct_rent_occ, freq = TRUE, breaks=25)
hist(df$pct_rent_occ, freq = TRUE, breaks=50)
hist(df$pct_rent_occ, freq = TRUE, breaks=75)

hist(df$pct_rent_occ, freq = FALSE)

hist(df$pct_rent_occ, freq = FALSE)

# Density Plots

plot(density(df$pct_rent_occ), main="Kernel Density Plot of Renter Occupancy")

plot(density(df$pct_rent_occ), col='blue', main="Kernel Density Plot of Renter Occupancy vs. Owner-Occupancy")

lines(density(df$pct_own_occ), col='red')
