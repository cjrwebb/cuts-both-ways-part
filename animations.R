# Cuts Both Ways Animations of Seesaw/SG
library(tidyverse)
library(gganimate)
library(ggimage)

gradients <- rev(seq(0, 3, 1))
gradients <- c(3, 0)

# Create seesaw in GGplot
ggplot() +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 5, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 5, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 5, colour = "#3B301B") +
  geom_abline(intercept = 5, slope = 4, size = 10, colour = "#26CFA5") +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 6, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 6, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 6, fontface = "bold") +
  ylim(c(-0.75, 13)) +
  xlim(c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank())


# Create decreasing slope animation

anim_test <- ggplot() +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(intercept = 5, slope = gradients, size = 30, colour = "#26CFA5") +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  ylim(c(-0.75, 13)) +
  xlim(c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  gganimate::transition_states(gradients, transition_length = 0.05, state_length = 0, wrap = FALSE)

animate(anim_test, fps = 30, height = 1600, width = 1600, renderer = gifski_renderer(loop = FALSE), duration = 2)
anim_save("seesaw_1.gif", path = "images/")

# Draw gradient on last

#Final frame


ggplot() +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(intercept = 5, slope = 3, size = 30, colour = "#26CFA5") +
  geom_segment(aes(y = 2, yend = 2, x = -1, xend = 0), size = 6, lty = 2) +
  geom_segment(aes(y = 2, yend = 5, x = 0, xend = 0), size = 6, lty = 2) +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  ylim(c(-0.75, 13)) +
  xlim(c(-1, 1)) +
  theme_minimal()# +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank())


  
xend_travel <- seq(-1, 0, 0.1)
yend_travel <- seq(2, 5, 0.3)
ggplot_objs_1 <- list()

pos_x1 <- c(-1, -1, -1, -1)
pos_x2 <- c(-1, 0, 0, 0)
pos_y2 <- c(2, 2, 5, 5)
pos_label <- c("", "", "", "1.7x")
pos_state <- c(1, 2, 3, 4)

seg_data <- tibble(pos_x1, pos_x2, pos_y2, pos_state)

seesaw2 <- ggplot(seg_data) +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(intercept = 5, slope = 3, size = 30, colour = "#26CFA5") +
  geom_segment(aes(y = 2, yend = 2, x = pos_x1, xend = pos_x2), size = 3, lty = 2) +
  geom_segment(aes(y = 2, yend = pos_y2, x = 0, xend = 0), size = 3, lty = 2) +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  annotate(geom = "text", x = -0.25, y = 3, label = pos_label, size = 18, fontface = "bold") +
  ylim(c(-0.75, 13)) +
  xlim(c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  gganimate::transition_states(pos_state, transition_length = 0.05, state_length = 0, wrap = FALSE) +
  ease_aes()

animate(seesaw2, fps = 30, height = 1600, width = 1600, renderer = gifski_renderer(loop = FALSE), duration = 3)
anim_save("seesaw_2.gif", path = "images/")

pos_x1_2 <- c(0, 0, 0, 0)
pos_x2_2 <- c(0, 1, 1, 1)
pos_y2_2 <- c(5, 5, 8, 8)
pos_label_2 <- c("", "", "", "1.7x")
pos_state_2 <- c(1, 2, 3, 4)

seg_data_2 <- tibble(pos_x1_2, pos_x2_2, pos_y2_2, pos_state_2)

seesaw3 <- ggplot(seg_data_2) +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(intercept = 5, slope = 3, size = 30, colour = "#26CFA5") +
  geom_segment(aes(y = 2, yend = 2, x = -1, xend = 0), size = 3, lty = 2, alpha = 0.6) +
  geom_segment(aes(y = 2, yend = 5, x = 0, xend = 0), size = 3, lty = 2, alpha = 0.6) +
  geom_segment(aes(y = 5, yend = 5, x = pos_x1_2, xend = pos_x2_2), size = 3, lty = 2) +
  geom_segment(aes(y = 5, yend = pos_y2_2, x = 1, xend = 1), size = 3, lty = 2) +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  annotate(geom = "text", x = -0.25, y = 3, label = "1.7x", size = 18, fontface = "bold", alpha = 0.6) +
  annotate(geom = "text", x = 0.75, y = 6, label = pos_label, size = 18, fontface = "bold") +
  ylim(c(-0.75, 13)) +
  xlim(c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  gganimate::transition_states(pos_state, transition_length = 0.05, state_length = 0, wrap = FALSE) +
  ease_aes()

animate(seesaw3, fps = 30, height = 1600, width = 1600, renderer = gifski_renderer(loop = FALSE), duration = 3)

anim_save("seesaw_3.gif", path = "images/")

# Fade out social gradients and "drop on" contextual effects

seesaw4_params <- tibble(
                         ypos_drop = c(18, 15, 13, 11, 9.5, 8.5),
                         slope_change = c(3, 3, 3, 3, 2.5, 2),
                         state_change = c(1, 2, 3, 4, 5, 6))


seesaw4 <- ggplot(seesaw4_params) +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(aes(intercept = 5, slope = slope_change), size = 30, colour = "#26CFA5") +
  # geom_segment(aes(y = 2, yend = 2, x = -1, xend = 0, alpha = alpha_change), size = 3, lty = 2) +
  # geom_segment(aes(y = 2, yend = 5, x = 0, xend = 0, alpha = alpha_change), size = 3, lty = 2) +
  # geom_segment(aes(y = 5, yend = 5, x = 0, xend = 1, alpha = alpha_change), size = 3, lty = 2) +
  # geom_segment(aes(y = 5, yend = 8, x = 1, xend = 1, alpha = alpha_change), size = 3, lty = 2) +
  # exit_fade() +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  # annotate(geom = "text", x = -0.25, y = 3, label = "3x", size = 18, fontface = "bold", alpha = seesaw4_params$alpha_change) +
  # annotate(geom = "text", x = 0.75, y = 6, label = "3x", size = 18, fontface = "bold", alpha = seesaw4_params$alpha_change) +
  geom_image(aes(x = 0.8, y = ypos_drop), angle = 45, image = "images/icons8-courthouse.png", size = 0.2, asp = 1) +
  coord_cartesian(ylim = c(-0.75, 13), xlim = c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  ggeasy::easy_remove_legend() +
  gganimate::transition_states(state_change, transition_length = 0.05, state_length = 0, wrap = FALSE) +
  ease_aes()


animate(seesaw4, fps = 30, height = 1600, width = 1600, renderer = gifski_renderer(loop = FALSE), duration = 2.5)

anim_save("seesaw_4.gif", path = "images/")

# Draw line on seesaw with greater equality (social gradient = 1.4)

seesaw5_params <- tibble(
  pos_x1_start = c(-1, -1, -1, -1, -1),
  pos_x1_end = c(-1, 0, 0, 0, 0),
  pos_x2_start = c(0, 0, 0, 0, 0),
  pos_x2_end = c(0, 1, 1, 1, 1),
  pos_y1_start = c(3, 3, 3, 3, 3),
  pos_y1_end = c(3, 3, 5, 5, 5),
  pos_y2_start = c(5, 5, 5, 5, 5),
  pos_y2_end = c(5, 5, 7, 7, 7),
  trans_state = c(1, 2, 3, 4, 5),
  text_lab_alpha = c(0, 0, 0, 0.7, 1),
  text_lab = c("", "", "", "1.4x", "1.4x")
)


seesaw5 <- ggplot(seesaw5_params) +
  geom_segment(aes(x = 0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.25, xend = 0, y = 0, yend = 5), size = 12, colour = "#3B301B") +
  geom_segment(aes(x = -0.3, xend = 0.3, y = 0, yend = 0), size = 12, colour = "#3B301B") +
  geom_abline(aes(intercept = 5, slope = 2), size = 30, colour = "#26CFA5") +
  geom_segment(aes(y = 3, yend = 3, x = pos_x1_start, xend = pos_x1_end), size = 3, lty = 2) +
  geom_segment(aes(y = pos_y1_start, yend = pos_y1_end, x = 0, xend = 0), size = 3, lty = 2) +
  geom_segment(aes(y = 5, yend = 5, x = pos_x2_start, xend = pos_x2_end), size = 3, lty = 2) +
  geom_segment(aes(y = pos_y2_start, yend = pos_y2_end, x = 1, xend = 1), size = 3, lty = 2) +
  annotate(geom = "text", x = -1, y = -0.75, label = "Low\nDeprivation", hjust = 0, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 0, y = -0.75, label = "Average\nDeprivation", hjust = 0.5, size = 10, fontface = "bold") +
  annotate(geom = "text", x = 1, y = -0.75, label = "High\nDeprivation", hjust = 1, size = 10, fontface = "bold") +
  geom_text(aes(x = -0.25, y = 3.5, label = text_lab, alpha = text_lab_alpha), size = 18, fontface = "bold") +
  geom_text(aes(x = 0.75, y = 5.5, label = text_lab, alpha = text_lab_alpha), size = 18, fontface = "bold") +
  geom_image(aes(x = 0.8, y = 8.5), image = "images/icons8-courthouse.png", size = 0.2, asp = 1) +
  coord_cartesian(ylim = c(-0.75, 13), xlim = c(-1, 1)) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank()) +
  ggeasy::easy_remove_legend() +
  gganimate::transition_states(trans_state, transition_length = 0.05, state_length = 0, wrap = FALSE) +
  ease_aes()

animate(seesaw5, fps = 30, height = 1600, width = 1600, renderer = gifski_renderer(loop = FALSE), duration = 3.5)

anim_save("seesaw_5.gif", path = "images/")

# CWIP plot?

cwi_rates <- tibble(depr = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
       cpp = c(9, 14, 21, 23, 32, 35, 47, 51, 71, 113, 1, 5, 8, 13, 17, 33, 39, 55, 74, 114), 
       cpp_text = as.character(c(9, 14, 21, 23, 32, 35, 47, 51, 71, 113, 1, 5, 8, 13, 17, 33, 39, 55, 74, 114)), 
       cla = c(12, 14, 23, 29, 29, 40, 55, 67, 88, 133, 7, 12, 16, 22, 31, 39, 54, 63, 108, 165),
       country = c(rep("England", 10), rep("Wales", 10)))

cwi_rates_init <- tibble(depr = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    cpp = rep(1, 20), 
                    cla = rep(1, 20),
                    cpp_text = as.character(c(9, 14, 21, 23, 32, 35, 47, 51, 71, 113, 1, 5, 8, 13, 17, 33, 39, 55, 74, 114)), 
                    country = c(rep("England", 10), rep("Wales", 10)),
                    trans_state = 1)

cwip_plot <- cwi_rates %>%
  mutate(trans_state = 2) %>%
  add_row(cwi_rates_init) %>%
  ggplot() +
  geom_bar(aes(x = as.factor(depr), y = cpp, group = country, fill = country, text = paste("CPP rate:", cpp, "per 10,000 -", country)), stat = "identity", position = "dodge") +
  geom_text(aes(x = as.factor(depr), y = cpp + 2, group = country,  label = cpp_text, colour = country), stat = "identity", position = position_dodge(width = 1), size = 9, fontface = "bold") +
  ggtitle(str_wrap("", 80)) +
  scale_fill_manual(values = c("#26CFA5", "#F8C500")) +
  scale_colour_manual(values = c("#26CFA5", "#F8C500")) +
  ylab("Child Protection Plans per 10,000 Children") +
  xlab("Deprivation Decile of Neighbourhoods (10 = Most Deprived)") +
  ggeasy::easy_add_legend_title("Country") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), text = element_text(size = 24)) +
  ylim(c(0,120)) +
  gganimate::transition_states(trans_state, transition_length = 0.05, state_length = 0, wrap = FALSE) +
  ease_aes()

animate(cwip_plot, fps = 30, height = 960, width = 1920, renderer = gifski_renderer(loop = FALSE), duration = 2)

anim_save("cpp_graph.gif", path = "images/")
