# Load libraries
library(tidyverse)
library(hrbrthemes)
library(scales)    # for comma_format()
library(patchwork) # for arranging plots side by side

# --- Mortality plot ---
df <- data.frame(
  HFpEF = 3.0,
  HFrEF = 3.9
)

df <- pivot_longer(df, everything(), names_to = "HF", values_to = "Percentage")
df$HF <- factor(df$HF, levels = c("HFpEF", "HFrEF"))

max_val <- max(df$Percentage)
y_max <- max_val * 1.1  # 10% padding

mortality <- ggplot(df, aes(x = HF, y = Percentage, fill = HF)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.9),
            size = 8, color = "white", face = "bold") +
  geom_smooth(aes(group = 1), color = "black", linetype = "solid",
              size = 1, se = FALSE, show.legend = FALSE) +
  theme_ipsum(base_size = 20) +
  labs(y = "In-Hospital Mortality (%)") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.6),
    axis.line.y = element_line(color = "black", size = 0.6),
    axis.text.x = element_text(vjust = -1, hjust = 0.5, size = 20),
    axis.text.y = element_text(vjust = 0, hjust = 1, size = 20),
    axis.ticks = element_line(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 24, face = "bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), # less space
    legend.position = "none",
    panel.border = element_blank(),
    text = element_text(size = 20)
  ) +
  scale_y_continuous(breaks = seq(0, ceiling(y_max), by = 1), limits = c(0, y_max), expand = c(0, 0)) +
  scale_x_discrete(labels = c("HFpEF" = "HFpEF", "HFrEF" = "HFrEF")) +
  scale_fill_manual(values = c("HFpEF" = "#0172b6", "HFrEF" = "#b34745"))

# --- Length of Stay plot ---
df <- data.frame(
  HFpEF = 7.0,
  HFrEF = 7.6
) |>
  pivot_longer(everything(), names_to = "HF", values_to = "Value") |>
  mutate(HF = factor(HF, levels = c("HFpEF", "HFrEF")))

max_val <- max(df$Value)
y_max <- max_val * 1.1

LOS <- ggplot(df, aes(HF, Value, fill = HF)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = round(Value, 1)),
            vjust = 2, colour = "white", size = 8, fontface = "bold") +
  theme_ipsum(base_size = 20) +
  labs(y = "Mean Length of Stay (days)", x = NULL) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, ceiling(y_max), 1), limits = c(0, y_max)) +
  scale_fill_manual(values = c("HFpEF" = "#0172b6", "HFrEF" = "#b34745")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.6),
    axis.line.y = element_line(color = "black", size = 0.6),
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 24, face = "bold", margin = margin(t = 0, r = 30, b = 0, l = 0)), # more space
    axis.title.x = element_blank()
  )

# --- Hospital Charges plot ---
df <- data.frame(
  HFpEF = 76088,
  HFrEF = 94365
) |>
  pivot_longer(everything(), names_to = "HF", values_to = "Value") |>
  mutate(HF = factor(HF, levels = c("HFpEF", "HFrEF")))

max_val <- max(df$Value)
y_max <- max_val * 1.1
y_breaks <- seq(0, ceiling(y_max / 10000) * 10000, by = 20000)

Charge <- ggplot(df, aes(HF, Value, fill = HF)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = comma(Value, accuracy = 1)),
            vjust = 2, colour = "white", size = 8, fontface = "bold") +
  theme_ipsum(base_size = 20) +
  labs(y = "Total Hospital Charges (USD)", x = NULL) +
  scale_y_continuous(expand = c(0, 0),
                     labels = scales::comma_format(accuracy = 1),
                     breaks = y_breaks,
                     limits = c(0, y_max)) +
  scale_fill_manual(values = c("HFpEF" = "#0172b6", "HFrEF" = "#b34745")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.6),
    axis.line.y = element_line(color = "black", size = 0.6),
    legend.position = "none",
    axis.text = element_text(size = 20),
    axis.title.y = element_text(size = 24, face = "bold", margin = margin(t = 0, r = 10, b = 0, l = 0)), # medium space
    axis.title.x = element_blank()
  )

# --- Combine plots side by side ---
combined_plot <- mortality + LOS + Charge + plot_layout(nrow = 1)

# Display combined plot
print(combined_plot)

# Save combined plot to TIFF
ggsave("Combined_Plots.jpeg", combined_plot, width = 18, height = 6, units = "in", bg = "white")
