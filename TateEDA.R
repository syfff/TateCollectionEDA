library(tidyverse)

movies <-
  read_csv('/Users/shenyifan/Desktop/Top_10_Movies_Worldwide_2013-2022.csv')

movies$Year <- as.factor(movies$Year)
movies$`Worldwide Box Office` =  as.numeric(gsub('[^-0-9.]', '', movies$`Worldwide Box Office`))
movies$`US Box Office` =  as.numeric(gsub('[^-0-9.]', '', movies$`US Box Office`))
movies$`US Opening Weekend Box Office` =  as.numeric(gsub('[^-0-9.]', '', movies$`US Opening Weekend Box Office`))

movies %>%
  filter(`Worldwide Box Office Rank` <= 6) %>%
  mutate(`Worldwide Box Office Rank` = as.factor(`Worldwide Box Office Rank`)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = `Worldwide Box Office`, shape = `Worldwide Box Office Rank`))

p1 <- movies %>%
  group_by(Year) %>%
  summarise(mean = mean(`Worldwide Box Office`)) %>%
  ggplot(aes(x = Year, y = mean)) +
  geom_point()

p2 <- movies %>%
  filter(`Worldwide Box Office Rank` <= 5) %>%
  group_by(Year) %>%
  summarise(mean = mean(`Worldwide Box Office`)) %>%
  ggplot(aes(x = Year, y = mean)) +
  geom_point()

p1 + p2

# p <-

subscribers.netflix <-
  c (4.143,
     5.448,
     7.084,
     8.909,
     11.064,
     13.926,
     16.709,
     20.366,
     22.184,
     24.745)

movies <- movies %>%
  mutate(porportion = `US Box Office` / `Worldwide Box Office`)


top5 <- movies %>%
  filter(`Worldwide Box Office Rank` <= 5) %>%
  group_by(Year) %>%
  summarise(mean_box = mean(`Worldwide Box Office`) / (10 ^ 9))


movies %>%
  group_by(Year) %>%
  summarise(mean_box = mean(`Worldwide Box Office`) / (10 ^ 9)) %>%
  ggplot(aes(x = Year, y = mean_box)) +
  geom_line(aes(group = 1, color = "#E50914")) +
  geom_point(
    aes(y = mean_box),
    shape = 16,
    colour = "black",
    size = 2
  ) +
  geom_line(aes(y = subscribers.netflix / 20, group = 1, color = "black")) +
  geom_point(
    aes(y = subscribers.netflix / 20),
    shape = 2,
    colour = "black",
    size = 2
  ) +
  theme_classic() + 
  scale_colour_manual(name="Legend",
                      labels = c("Box Office Revenue", "Netflix Subscribers"),
                      values = c("black", "#E50914")) +
  geom_vline(xintercept = 7.5, linetype = 3, size = 0.8) +
  annotate("text",
           x = 8.4,
           y = 1.45,
           label = "Covid-19 Outbreak") +
  scale_y_continuous(
    limits = c(0, 1.5),
    sec.axis = sec_axis( ~ . *20, name = "Total Worldwide Netflix Subscribers (Billion)")
  ) +
  ggtitle("How has the Covid-19 outbreak affected the annual global box office for the top 10 movies?") +
  labs(x = "Year",
       y = "Mean Global Box Office for Top 10 Movies Anually($Billion)") + 
  theme(axis.text = element_text(size = 11))  + 
  theme(axis.title = element_text(size = 14)) +
  theme(plot.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 12))  + 
  theme(legend.text = element_text(size = 11))  + 
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.8, linetype = "solid")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(vjust=2)) + 
  theme(legend.box.background = element_rect(colour = "black"))

movies %>%
  ggplot(aes(x = Year, y = `Worldwide Box Office`/(10^9))) +
  geom_boxplot(fill=c('steelblue')) +
  geom_line(aes(y = subscribers.netflix / 20, group = 1, color = "black")) +
  geom_point(
    aes(y = subscribers.netflix / 20),
    shape = 2,
    colour = "black",
    size = 2
  ) +
  theme_classic() + 
  scale_colour_manual(name="Legend",
                      labels = c("Box Office Revenue", "Netflix Subscribers"),
                      values = c("black", "#E50914")) +
  geom_vline(xintercept = 7.5, linetype = 3, size = 0.8) +
  annotate("text",
           x = 8.4,
           y = 1.45,
           label = "Covid-19 Outbreak") +
  scale_y_continuous(
    limits = c(0, 1.5),
    sec.axis = sec_axis( ~ . *20, name = "Total Worldwide Netflix Subscribers (Billion)")
  ) +
  ggtitle("How has the outbreak of Covid-19 influenced the top-10 movie box office?") +
  labs(x = "Year",
       y = "Mean Worldwide Box Office for Annual Top 10 Movies ($Billion)") + 
  theme(axis.text = element_text(size = 11))  + 
  theme(axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(size = 16)) + 
  theme(legend.title = element_text(size = 12))  + 
  theme(legend.text = element_text(size = 11))  + 
  theme( axis.line = element_line(colour = "black", 
                                  size = 0.8, linetype = "solid")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(vjust=2)) + 
  theme(legend.box.background = element_rect(colour = "black"))

ggplot(data, aes(x = day)) +
  
  geom_line(aes(y = temperature)) +
  geom_line(aes(y = price / coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(# Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( ~ . * coeff, name = "Second Axis"))

p + theme(
  plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
  axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
  axis.title.y = element_text(color = "#993333", size = 14, face = "bold")
)

movies %>%
  group_by(Year, Genre) %>%
  summarise(ratio = sum(`US Box Office`) / sum(`Worldwide Box Office`)) %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = Year, y = ratio, fill = Genre),
    alpha = 3 / 5,
    stat = "identity"
  )

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = movies, mapping = aes(x = Year, fill = Genre)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = `Production Method`)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = Distributor)) +
  geom_bar(position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = `Creative Type`)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies, mapping = aes(x = Year, fill = Source)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = `Primary Language`)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = `Top Non-US Market`)) +
  geom_bar(alpha = 3 / 5, position = "fill")

ggplot(data = movies,
       mapping = aes(x = Year, fill = `Primary Language`)) +
  geom_point(alpha = 3 / 5, position = "fill")

movies %>%
  group_by(Year, Genre) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Year, y = n, class = Genre)) +
  geom_bar()

movies %>%
  filter(`US Box Office` != 0)  %>%
  summarise(rank = `Worldwide Box Office Rank`,
            Year = Year,
            ratio = `US Opening Weekend Box Office` / `US Box Office`) %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = ratio, shape = as.factor(rank)))

movies %>%
  filter(`Worldwide Box Office Rank` == 1) %>%
  ggplot() +
  geom_bar(mapping = aes(x = Year, y = `Worldwide Box Office`, fill =))
