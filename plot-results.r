conf_df <- data.frame(
  Predicted = c(tp + fp, tn + fn),
  Actual = c(tp + fn, tn + fp),
  Freq = c(tp + fn, tn + fp),
  Percent = c((tp + fn) / (tp + fp), (tn + fp) / (tn + fn))
)

# Plot
plot_title <- "CART Confusion Matrix"
#ggplot(conf_df, aes(x = Predicted, y = Actual, fill=Freq)) +
#  geom_tile(color = "white") +
#  geom_text(aes(label = paste0(Freq, "\n(", Percent, "%)")), size = 5) +
#  scale_fill_gradient(low = "lightblue", high = "steelblue") +
#  labs(
#    title = plot_title,
#    x = "Predicted Label",
#    y = "Actual Label"
#  ) +
#  theme_minimal(base_size = 14)
ggplot(data = as.data.frame(confusion_matrix_for_plot),
       mapping = aes(x = opt_pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  #geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  #geom_text(aes(label = paste0(Freq, "\n(", Percent, "%)")), size = 5) +
  geom_text(aes(label = paste0(Freq)), size=5) +
  scale_fill_gradient(low = "lightblue",
                      high = "steelblue",
                      trans = "log") + # if your results aren't quite as clear as the above example
  labs(
        title = plot_title,
        x = "Predicted",
        y = "Actual"
      ) +
      theme_minimal(base_size = 14)