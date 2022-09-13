# Mobile IDV Capstone 
# Author: David Brandtner
# Date: 2022-09-13


# Loading required packages
require_install <- function(x){
  for (i in x){
    print(i)
    if(!require(i, character.only = TRUE)) {
      install.packages(i)
    }
  }
}

requiredPackages <- c("tidyverse", "dplyr", "caret", "knitr", "patchwork",
                      "kableExtra", "Hmisc", "rje")
require_install(requiredPackages)
lapply(requiredPackages, library, character.only = TRUE)

###############################################################################
# Downloading and Train/Test datasets creation
###############################################################################

train <- read.csv("train.csv")
closeAllConnections() #close read file connection for memory usage

# if using R 3.5 or earlier, use `set.seed(1)`
set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = train$price_range, times = 1, 
                                  p = 0.2, list = FALSE)
test <- train[test_index, ]
train <- train[-test_index, ]

rm(test_index)


###############################################################################
# Data Exploration and preprocessing
###############################################################################

# check for outcome classes balance:
table(train$price_range)

# NA check:
sum(is.na(train))

# structure compact display for train dataset:
str(train)


###########################################################
# Categorical predictor analysis:

# plotting categorical predictors frequencies:
cat <- c("blue", "dual_sim", "four_g", "three_g", "touch_screen",
          "wifi")

plot_categorical <- train %>% select(all_of(cat), "price_range") %>% 
  dplyr::mutate(across(everything(), ~ as.factor(.x))) %>%
  reshape2::melt(measure.vars = cat) %>%
  ggplot(aes(x=value, fill=value)) + geom_bar(alpha = 0.9) +
  labs(x = "Category", y = "Count", 
       title = "Categorical predictors frequencies", 
       subtitle = "1 = present / 0 = not present") +
  scale_fill_manual(values=c("#0C6291", "#A63446")) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 11.5, face = "italic"), 
        plot.caption = element_text(size = 15, hjust = 0),
        legend.position = "none") +
  facet_grid(rows = vars(price_range),
             cols = vars(variable),
             scales = "free_x")

plot_categorical  


###########################################################
# Preprocessing datasets:

# removing categorical features:
train_pp <- train[, !names(train) %in% cat]
test_pp <- test[, !names(test) %in% cat]


###########################################################
# Continuous predictor analysis:

# quick display of continuous predictors:
summary(train_pp)

# 0 as NA values removal in fc, pc, px_height and sc_w predictors:
train_pp %>% 
  filter(fc == 0 | pc == 0 | px_height == 0 | sc_w == 0) %>%
  select(fc, pc, px_height, sc_w) %>% nrow()

# table with 0 values frequency in target predictor:
zero_tab <- train_pp %>% select (fc, pc, px_height, sc_w) %>%
  plyr::ldply(function(c) sum(c == 0))
colnames(zero_tab) <- c("predictor", "0_freq")
zero_tab %>% 
  kableExtra::kbl(caption = "0 occurencies in selected predictors") %>%
  kable_paper("hover", full_width = F)


###########################################################
# Dealing with 0 null values:

# target predictors:
fill0_t <- c("fc", "pc", "px_height", "sc_w")

# function to substitute 0 values in target predictors with the mean
# of its belonging price class:
fill0_fun <- function(t, df){ 
  for (j in 1:length(t)){  
    for (i in 1:4){
      # first it calculates feature mean value (excluding the 0s)       
      fill <- df %>% filter(price_range == i-1 & .data[[t[[j]]]] != 0) %>% 
        summarise(mean = round(mean(.data[[t[[j]]]]), 0))
      # and then substitute it to the 0 = NA positions 
      df <- df %>% mutate("{t[j]}" := 
                    ifelse(price_range == i-1 & .data[[t[[j]]]] == 0, fill$mean,
                                   .data[[t[[j]]]]))
    }
  }
  return(df)
}


###########################################################
# Preprocessing datasets:

# deal with 0 values applying fill0_fun():
train_pp <- fill0_fun(fill0_t, train_pp)
test_pp <- fill0_fun(fill0_t, train_pp)
rm(fill0_t)


###########################################################
# Preprocessing datasets:

# new predictors setting up: mega_px and sc_ppi
train_pp <- train_pp %>% mutate(mega_px = (px_width*px_height)/10^6, 
                                sc_ppi = sqrt(px_width^2+px_height^2)/
                                  (sqrt(sc_w^2+sc_h^2)/2.54))

test_pp <- test_pp %>% mutate(mega_px = (px_width*px_height)/10^6, 
                              sc_ppi = sqrt(px_width^2+px_height^2)/
                                (sqrt(sc_w^2+sc_h^2)/2.54))


############################################################
# Pearson's correlation of continuous predictors -
# calculation and plotting:

# function for correlation calculation and edit output:
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}

# function for edit results of cors() as heatmap input:
formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA),
           r_if_sig = ifelse(P <.05, r, NA)) 
}

# plotting correlation heatmap using formatted_cors() as input:
formatted_cors(train_pp) %>% 
  ggplot(aes(measure1, measure2, fill = r, label = round(r_if_sig, 2))) +
  geom_tile(col = "black") +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", 
       title="Correlations in train_pp only for continuous features", 
       subtitle="Only significant (p<0.05) 
       Pearson's correlation coefficients shown") + 
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",
                       high="#A63446", limits=c(-1,1)) +
  geom_text(size=3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 ,vjust = 1)) +
  theme()


###########################################################
# Preprocessing datasets:

# predictors selection after correlation results and
# distribution transformation for mega_px and sc_ppi
train_pp <- train_pp %>% 
  select(price_range, ram, battery_power, mega_px, px_height, px_width,
         sc_ppi) %>% mutate(price_range = as.factor(price_range), 
                            mega_px_t = sqrt(mega_px), sc_ppi_t = log(sc_ppi))

test_pp <- test_pp %>% 
  select(price_range, ram, battery_power, mega_px, px_height, px_width,
         sc_ppi) %>% mutate(price_range = as.factor(price_range), 
                            mega_px_t = sqrt(mega_px), sc_ppi_t = log(sc_ppi))


########################################################
# Predictors distribution analysis plotting a box plot
# overlapped with a violin plot:

# subtitles plot:
f_subt <- c("ram", "battery_power", "mega_px", "px_height", "px_width", 
            "sc_ppi", "mega_px - normalized", "sc_ppi - normalized")

# y labels plot:
f_ylab <- c("bytes", "mAh", "MPixel", "pixels","pixels", "PPI", 
            "sqrt of Mpixel", "log10 of PPI")


# plot template function:
plots_function <- function(data, column, subt, ylab){
      ggplot(data, aes(price_range, column, 
                 group = price_range, fill = price_range)) +
      geom_boxplot(alpha = 0.6) + geom_violin(alpha = 0.2) +
      labs(subtitle = subt, fill = "Price Class") +
      scale_fill_manual(values=c("#0C6291","#969696","#884791", "#A63446")) +
      xlab("Price Class") +
      ylab(ylab) +
      theme_classic() +
      theme(plot.subtitle = element_text(size = 17, face = "italic"), 
            axis.title = element_text(size = 11.5), 
            legend.title = element_text(size = 11.5))
   }

# loop to produce actual plots, saving them in a list:
plot_list <- list()
for (n in 2:9){
  plot <- eval(substitute(plots_function(train_pp, train_pp[,n], 
          f_subt[n-1], f_ylab[n-1]), list(n = n)))
  plot_list[[n-1]] <- plot
}

# final panel plot composition with patchwork library:
plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]] + 
  plot_list[[6]] +   plot_list[[7]] + plot_list[[5]] + plot_list[[8]] +
  guide_area() + plot_layout(ncol = 3, guides = "collect") + 
  plot_annotation(title = "Predictors distribution and outliers", 
                  caption = "Fig.3", tag_levels = "A") & 
  theme(plot.title = element_text(size = 20, face = "bold"), 
        plot.caption = element_text(size = 23, hjust = 0.1))


###############################################################################
# Modeling 
###############################################################################

########################################################
# Common setting up for KNN and linear SVM:

# predictors selection with mega_px_t and sc_ppi_t:
ps <- subset(colnames(train_pp[-1]), 
             str_ends(colnames(train_pp[-1]), "_p+.",negate = T))

# create predictors selection power set (removing first empty element):
power_list <- powerSet(ps)[-1]

# trainControl() parameters for cross-validation setting up:
set.seed(1)
control <- trainControl(method = "repeatedcv", number = 10, p = 0.9,
                        repeats = 3)


###########################################################
# Preprocessing datasets:

# predictors min/max normalization:
preprocessParams_train <- preProcess(train_pp[ps], method=c("range"))
train_pp[ps] <- predict(preprocessParams_train, train_pp[ps])

preprocessParams_test <- preProcess(test_pp[ps], method=c("range"))
test_pp[ps] <- predict(preprocessParams_test, test_pp[ps])


###########################################################
# KNN Models Training:
###########################################################

# k tuning grid setting up:
kgrid = seq(3, 31, 2)

# summary results dataframe setting up:
rs <- data.frame(subset = character(), model_id = numeric(),
                 pred_nr = numeric(), k = integer(), accuracy = numeric())

# loop for try out all possible model from power set subsets:
top_acc = 0
best_train_knn <- list()

for (i in 1:length(power_list)){
  set.seed(1)
  train_knn <- train(x = train_pp[power_list[[i]]], y = train_pp$price_range,
                     method = "knn",
                     tuneGrid = data.frame(k = kgrid),
                     trControl = control)

    # saving best train_knn object
  if(max(train_knn$results$Accuracy) > top_acc){
        top_acc = max(train_knn$results$Accuracy)
        best_train_knn = train_knn}
  
  # updating results dataframe
  rs <- rs %>% add_row(
    subset = paste(power_list[[i]], collapse = " + "),
    model_id = i,
    pred_nr = length(power_list[[i]]),
    k = train_knn$finalModel$k,
    accuracy = max(train_knn$results$Accuracy)
    )
}

# print table with training results for best models 
# grouped by nr of predictors used:
rs_k <- rs %>% arrange(desc(accuracy)) %>% group_by(pred_nr) %>% slice(1)
rs_k %>%  kableExtra::kbl(booktabs = T, caption = 
 "KNN|Summary of best performing models. In red model19 the best performing 
 amongst all.") %>%
  kable_styling(latex_option = "scale_down", full_width = F) %>%
  column_spec(4, border_left = T) %>%
  row_spec(which(rs_k$accuracy == max(rs_k$accuracy)),
           bold = T, background = "#bd6a77")


###########################################################
# KNN Results
###########################################################

# class prediction with model19:
y_hat_knn <- predict(best_train_knn, test_pp[ps])

# confusion matrix and statistics:
cm_knn <- confusionMatrix(y_hat_knn, test_pp$price_range, mode = "everything")

# print best results table:
model19_rs <- rs[,2:5] %>% filter(model_id == 19)
model19_rs <- model19_rs %>% filter(row_number() == 1) %>% bind_rows(model19_rs)
model19_rs$dataset <- c("train_pp", "test_pp")
model19_rs[2,4] <- cm_knn$overall["Accuracy"]
model19_rs %>% kableExtra::kbl(booktabs = T, 
  caption = "KNN|Model19 performance: final overall Accuracy in red.") %>%
  kable_styling(full_width = F) %>% 
  row_spec(2, bold = T, background = "#bd6a77")

# print confusion matrix table:
cm_knn$table %>% kableExtra::kbl(booktabs = T, 
caption = "KNN|Multiclass confusion matrix:\ncol/reference, row/prediction") %>%
  kable_styling(full_width = F)

# print selected performance metrics by class table:
c_knn <- as.data.frame(cm_knn$byClass) %>% select(Sensitivity, Specificity, F1)
c_knn %>% kableExtra::kbl(booktabs = T, 
  caption = "KNN Model19|Selected performance metrics by class.") %>%
  kable_styling(full_width = F) %>%
  column_spec(4, border_left = T, bold = T)

###########################################################
# Linear SVM Models Training:
###########################################################

# C tuning grid setting up
Cgrid = expand.grid(C = seq(2, 2.3, length = 5))

# summary results dataframe setting up:
rs2 <- data.frame(subset = character(), model_id = numeric(),
                  pred_nr = numeric(), C = numeric(), accuracy = numeric())

# loop for try out all possible model from power set subsets:
top_acc = 0
best_train_svm <- list()

for (i in 1:length(power_list)){
  set.seed(1)
  
  train_svm <- train(price_range ~ .,
                     data = cbind(train_pp["price_range"], 
                                  train_pp[power_list[[i]]]),
                     tuneGrid = Cgrid,
                     method = "svmLinear",
                     trControl = control)
  
  
  # saving best train_knn object
  if(max(train_svm$results$Accuracy) > top_acc){
    top_acc = max(train_svm$results$Accuracy)
    best_train_svm = train_svm}
  
  # updating results dataframe
  rs2 <- rs2 %>% add_row(
    subset = paste(power_list[[i]], collapse = " + "),
    model_id = i,
    pred_nr = length(power_list[[i]]),
    C = train_svm$bestTune$C,
    accuracy = max(train_svm$results$Accuracy)
  )
}

# print table with training results for best models 
# grouped by nr of predictors used:
rs_k2 <- rs2 %>% arrange(desc(accuracy)) %>% group_by(pred_nr) %>% slice(1)
rs_k2 %>%  kableExtra::kbl(booktabs = T, caption=
                             "SVM|Summary of best performing models
. In red model63 the best performing amongst all.") %>%
  kable_styling(latex_option = "scale_down", full_width = F) %>%
  column_spec(4, border_left = T) %>%
  row_spec(which(rs_k2$accuracy == max(rs_k2$accuracy)),
           bold = T, background = "#bd6a77")

###########################################################
# Linear SVM Results
###########################################################

# class prediction with model19:
y_hat_svm <- predict(best_train_svm, test_pp[ps])

# confusion matrix and statistics:
cm_svm <- confusionMatrix(y_hat_svm, test_pp$price_range, mode = "everything")

# print best results table:
model63_rs2 <- rs2[,2:5] %>% filter(model_id == 63)
model63_rs2 <- model63_rs2 %>% filter(row_number() == 1) %>% bind_rows(model63_rs2)
model63_rs2$dataset <- c("train_pp", "test_pp")
model63_rs2[2,4] <- cm_svm$overall["Accuracy"]
model63_rs2 %>% kableExtra::kbl(booktabs = T, 
caption = "SVM Model63| performance: final overall Accuracy in red.") %>%
kable_styling(full_width = F) %>% row_spec(2, bold = T, background = "#bd6a77")

# print confusion matrix table:
cm_svm$table %>% kableExtra::kbl(booktabs = T, 
caption = "SVM Model63|Multiclass confusion matrix:
\ncol/reference, row/prediction.") %>%
  kable_styling(full_width = F)

# print selected performance metrics by class table:
c_svm <- as.data.frame(cm_svm$byClass) %>% select(Sensitivity, Specificity, F1)
c_svm %>% kableExtra::kbl(booktabs = T, 
caption = "SVM Model63|Selected performance metrics by class.") %>%
  kable_styling(full_width = F) %>%
  column_spec(4, border_left = T, bold = T)