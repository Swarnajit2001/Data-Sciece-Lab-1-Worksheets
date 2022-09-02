load('IMDB_movies.Rdata')
View(dat)

#Q2
hist(dat$rating,
     main = 'Histogram of Ratings',
     xlab = 'Ratings')

hist(dat$rating,
     main = 'Histogram of Ratings',
     xlab = 'Ratings',
     col = 'white')

#Q3

boxplot(dat$rating,
        main = 'Boxplot of Ratings',
        ylab = 'Ratings',
        pch = 19)

boxplot(dat$rating,
        main = 'Boxplot of Ratings',
        ylab = 'Ratings',
        col = 'pink',
        pch = 19)

#Q4

boxplot(dat$men_rating, dat$women_rating,
        names = c('Men', 'Women'),
        ylab = 'Rating',
        main = 'Side-by-side boxplot for Men and Women ratings',
        pch = 19)

#Q5

h1 = hist(dat$men_rating, col = 'skyblue',
          main = 'Overlapping Histograms of ratings given by men and women',
          xlab = 'Ratings',
          xlim = range(c(dat$men_rating,dat$women_rating)))
h2 = hist(dat$women_rating, add =T,
          col = adjustcolor("red", alpha.f = .5))

legend('topright', c('Men','Women'), fill = c('skyblue',adjustcolor("red", alpha.f = .5)),
       title = 'INDEX')

#Q6

plot(dat$over.votes, dat$rating,pch = 19,
     xlab = 'Number of Votes',
     ylab = 'Ratings',
     main = 'Plot of Number of Votes VS Ratings',
     xlim = c(0, 2800000),
     ylim = c(8, 9.3))

#Q7

text(dat$over.votes[dat$rating > 8.9], dat$rating[dat$rating > 8.9], 
     dat$name[dat$rating > 8.9],pos = 3)

#Q8

color = ifelse(dat$year < 1996, 2, 7)

plot(dat$over.votes, dat$rating,pch = 19,
     xlab = 'Number of Votes',
     ylab = 'Ratings',
     main = 'Plot of Number of Votes VS Ratings',
     xlim = c(0, 2800000),
     ylim = c(8, 9.3),
     col = color)
legend('bottomright', legend = c('Released before 1996', 'Released after 1996'),col = c(2,7), pch = 19,
       title = 'INDEX')
