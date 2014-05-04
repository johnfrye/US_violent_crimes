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
require("gtable")


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


#+ plot.US.vs.NHL, echo=FALSE
NHL.data <- fread("fightDB_NHL.csv") %.%
  filter(year >= 1960 & year <= 2010) %.%
  select(year, reg.perG)

combined.data <- merge(filter(crime.data, state == "United States"),
                       NHL.data, by = "year")

grid.newpage()

g1 <- ggplot(data = combined.data) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 0.4, color = "dodgerblue4"),
        axis.text.y = element_text(color = "dodgerblue4"),
        axis.ticks.y = element_line(color = "dodgerblue4"),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines")) +
  xlab("Year") +
  ylab("Violent crimes per\n100 000 inhabitants") +
  coord_cartesian(ylim = c(0, 1.025 * max(combined.data$rate, na.rm = TRUE))) +
  geom_line(aes(x = year, y = rate),
            size = 2,
            color = "dodgerblue4")

g2 <- ggplot(data = combined.data) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 0.6, color = "tomato3"),
        axis.text.y = element_text(color = "tomato3"),
        axis.ticks.y = element_line(color = "tomato3"),
        axis.title.x = element_text(vjust = 0),
        plot.background = element_rect(fill = "#F0F0F0", color = "#F0F0F0"),
        text = element_text(family = "Courier"),
        plot.margin = unit(rep(1, 4), "lines"),
        panel.background = element_rect(fill = NA)) +
  coord_cartesian(ylim = c(0, 1.025 * max(combined.data$reg.perG, na.rm = TRUE))) +
  ylab("Fights/game (NHL regular season)") +
  geom_line(aes(x = year, y = reg.perG), 
            size = 2,
            color = "tomato3")

g1 <- ggplot_gtable(ggplot_build(g1))
g2 <- ggplot_gtable(ggplot_build(g2))

pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

ia <- which(g2$layout$name == "ylab")
ylab <- g2$grobs[[ia]]
g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ylab, pp$t, length(g$widths) - 1, pp$b)

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
           label = c("GRAPHZOO.TUMBLR.COM", "SOURCE: QUANDL, DROPYOURGLOVES"),
           color = "white", hjust = c(0.1, 0.9),
           size = 4, family = "Avenir Next Condensed")

png("US_violent_crime_rate_vs_violence_in_NHL.png", width = 800, height = 600)
grid.arrange(g, banner, heights = c(1, .05))
dev.off()

#+ plot.US.map, echo=FALSE
tmp <- as.data.table(map_data("state"))

ggplot(data = map_data("state"), 
       aes(x = long, 
           y = lat, 
           group = group)) + 
  theme_minimal() + 
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        plot.margin = unit(c(0,0,-1,-1), "lines")) +
  coord_fixed(ratio = 1) + 
  geom_polygon(color = "white", fill = "dodgerblue4")





