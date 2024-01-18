gg_box_env_fbi_trend <- function(df,
                                 sel_metric = NA,
                                 var_y = value,
                                 var_x = trend,
                                 var_facet,
                                 var_metric,
                                 y_log = TRUE)

{
  var_x <- enquo(var_x)
  var_y <- enquo(var_y)
  var_facet <- enquo(var_facet)
  var_metric <- enquo(var_metric)
  
  if(!is.na(sel_metric)) {df <- df %>% filter(!!var_metric == sel_metric)}

g <- ggplot(data = df,
       aes(x = !!var_x,
           y = !!var_y,
           fill = !!var_x)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("red", "darkgreen", "grey50")) +
  facet_wrap(vars(!!var_facet),
             scales = "free_y") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = "",
       y = "",
       fill = "FBI trend")

if(!is.na(sel_metric)) {g <- g + ggtitle(sel_metric)}

if(y_log) {g <- g + scale_y_continuous(trans = "log10")}

g

}
