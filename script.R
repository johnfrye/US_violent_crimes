#+ license, echo=FALSE
# 
# Copyright (C) 2014 Simon Garnier
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#+ libraries, echo=FALSE
require("Quandl")
require("data.table")
require("dplyr")
require("ggplot2")
require("maps")
require("gridExtra")

#+ load.data, echo=FALSE
tmp <- Quandl("FBI_UCR/USCRIME_TYPE_VIOLENTCRIMERATE")
crime.data <- data.table(year = year(tmp$Year),
                         state = stack(tmp[,-1])$ind,
                         rate = stack(tmp[,-1])$values)

#+ plot.US.vs.time, echo=FALSE
graph <- ggplot(data = filter(crime.data, state == "United States"),
                aes(x = year,
                    y = rate,
                    alpha = rate)) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  scale_alpha(guide = "none") +
  coord_cartesian(ylim = c(0, 800)) +
  ylab("Violent crimes per\n100 000 inhabitants") +
  xlab("Year") +
  geom_path(size = 2, color = "dodgerblue4") + 
  geom_point(size = 6, color = "dodgerblue4")

inset <- ggplotGrob(ggplot(data = map_data("state") , 
                           aes(x = long, 
                               y = lat, 
                               group = group)) + 
                      theme_minimal() + 
                      theme(line = element_blank(),
                            text = element_blank(),
                            title = element_blank(),
                            plot.margin = unit(c(0,0,-1,-1), "lines")) +
                      coord_fixed(ratio = 1) + 
                      geom_polygon(color = "dodgerblue4", fill = "dodgerblue4"))

graph <- graph + annotation_custom(grob = inset,
                                   xmin = 1980, xmax = 2010, ymax = 300)

banner <- ggplot(data = data.table(x = 0, y = 0),
                 aes(x = x, y = y)) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines"),
        panel.background = element_rect(fill = "grey40", color = "grey40")) + 
  geom_text(label = "") + 
  xlim(0, 10) +
  annotate("text", x = c(0, 10), y = 0, 
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: QUANDL"),
           color = "white", hjust = c(0.1, 0.8),
           size = 4, family = "Avenir Next Condensed")

png("US_violent_crime_rate_over_time.png", width = 800, height = 600)
grid.arrange(graph, banner, heights = c(1, .05))
dev.off()

#+ plot.All.States, echo=FALSE
tmp <- crime.data %.%
  filter(state != "United States" & year == 2010) %.%
  mutate(state = factor(state, levels = state[order(rate)]))

graph <- ggplot(data = tmp,
                aes(x = state,
                    y = rate,
                    alpha = rate)) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_line(color = "#00000050"),
        panel.grid.minor = element_line(color = "#00000012", linetype = 2),
        axis.title.y = element_text(vjust = 0.4),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  scale_alpha(guide = "none") +
  coord_flip() +
  xlab("US state") +
  ylab("Violent crimes per\n100 000 inhabitants (2010)") +
  guides(fill = FALSE) +
  geom_bar(stat = "identity", color = "white", fill = "dodgerblue4")

inset <- ggplotGrob(ggplot(data = map_data("state") , 
                           aes(x = long, 
                               y = lat, 
                               group = group)) + 
                      theme_minimal() + 
                      theme(line = element_blank(),
                            text = element_blank(),
                            title = element_blank(),
                            plot.margin = unit(c(0,0,-1,-1), "lines")) +
                      coord_fixed(ratio = 1) + 
                      geom_polygon(color = "dodgerblue4", fill = "dodgerblue4"))

graph <- graph + annotation_custom(grob = inset,
                                   ymin = 300, ymax = 1400, xmax = 20)

banner <- ggplot(data = data.table(x = 0, y = 0),
                 aes(x = x, y = y)) +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines"),
        panel.background = element_rect(fill = "grey40", color = "grey40")) + 
  geom_text(label = "") + 
  xlim(0, 10) +
  annotate("text", x = c(0, 10), y = 0, 
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: QUANDL"),
           color = "white", hjust = c(0.1, 0.8),
           size = 4, family = "Avenir Next Condensed")

png("US_violent_crime_rate_by_state_2010.png", width = 600, height = 800)
grid.arrange(graph, banner, heights = c(1, .05))
dev.off()