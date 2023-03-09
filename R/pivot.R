# data <- data_progress$Corn
#
# region <- Region(name = "nebraska", type = "us state",
#                  div = c(country = "United States", state = "Nebraska"))
#
# # Create a model
# object1 <- new("ProgressBM",
#                region = region,
#                crop = "Corn",
#                data = data,
#                formula = "CumPercentage ~ Time + agdd")
#
# stages = get_stages(object1)
#
# # data %>%
# #   dplyr::select(-c("CumPercentage")) %>%
# #   tidyr::pivot_wider(names_from = Stage, values_from = Percentage) %>%
# #   dplyr::select(Preseason, Planted, dayl, gdd)
#
#
# data_wide <- data %>%
#   dplyr::select(-c("CumPercentage")) %>%
#   tidyr::pivot_wider(names_from = Stage, values_from = Percentage)
#
# # d2 <- data %>%
# #   dplyr::select(c("Stage","CumPercentage", "dayl")) %>%
# #   tidyr::pivot_wider(names_from = Stage, values_from = CumPercentage)
#
# data_long <- data_wide %>%
#   tidyr::pivot_longer(cols = levels(stages),
#                       names_to = 'Stage',
#                       values_to = 'Percentage')
