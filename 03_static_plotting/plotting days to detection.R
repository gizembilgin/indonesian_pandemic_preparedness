
days_to_detection_df = crossing(
  this_outcome = c("deaths","presentations"),
  this_outcome_threshold = c(1,2,5,25,100),
  this_gen_interval = 14,
  this_IR_outcome = c(0.01,0.1,0.25,0.5),
  this_develop_outcome = 14,
  this_R0 = c(2,3,4),
  days_to_detection = NA
)
for (i in 1:nrow(days_to_detection_df)){
  days_to_detection_df$days_to_detection[i] = 
    estimate_days_to_detection(outcome = days_to_detection_df$this_outcome[i],
                               outcome_threshold = days_to_detection_df$this_outcome_threshold[i],
                               gen_interval = days_to_detection_df$this_gen_interval[i],
                               IR_outcome = days_to_detection_df$this_IR_outcome[i],
                               develop_outcome = days_to_detection_df$this_develop_outcome[i],
                               R0 = days_to_detection_df$this_R0[i])
}

days_to_detection_df <- days_to_detection_df %>%
  mutate(this_R0 = as.character(this_R0),
         label = case_when(
           this_outcome == "presentations" ~ "presentations",
           TRUE ~ paste0("deaths (IFR = ",this_IR_outcome,")")
         ))

ggplot(days_to_detection_df) + 
  geom_point(aes(x=days_to_detection,y=this_outcome_threshold,color=this_R0)) +
  geom_line(aes(x=days_to_detection,y=this_outcome_threshold,color=this_R0)) +
  xlim(0,max(days_to_detection_df$days_to_detection)) + 
  facet_grid(label ~.) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(color = "R0") +
  xlab("days to detection") + 
  ylab("outcome threshold")
