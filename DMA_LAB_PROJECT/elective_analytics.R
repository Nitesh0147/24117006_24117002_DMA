
packages <- "ggplot2"

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cloud.r-project.org")
}

suppressWarnings(
  suppressPackageStartupMessages(
    library(ggplot2)
  )
)

# 1. Load datasets

choose_csv <- function(label) {
  cat("Select:", label, "\n")
  tryCatch(
    file.choose(),
    error = function(e) stop("File selection cancelled. Run the script again.")
  )
}

student_train_path <- choose_csv("Student training CSV")
student_test_path <- choose_csv("Student testing CSV")
resource_train_path <- choose_csv("Resource training CSV")
resource_test_path <- choose_csv("Resource testing CSV")

setwd(dirname(student_train_path))

student_train <- read.csv(student_train_path, stringsAsFactors = FALSE)
student_test <- read.csv(student_test_path, stringsAsFactors = FALSE)
resource_train <- read.csv(resource_train_path, stringsAsFactors = FALSE)
resource_test <- read.csv(resource_test_path, stringsAsFactors = FALSE)

prefs <- paste0("P", 1:8)

# 2. Data preprocessing and validation

student_columns <- c(
  "Student_ID", "Semester", "CGPA",
  prefs, "Allocated_Elective"
)

resource_columns <- c(
  "Semester", "Elective_ID", "Elective_Name",
  "Current_Seats", "Classroom_Capacity_Per_Section",
  "Faculty_Available", "Faculty_Capacity_Per_Section",
  "Existing_Sections"
)

check_columns <- function(data, required) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(paste("Missing columns:", paste(missing, collapse = ", ")))
  }
}

check_columns(student_train, student_columns)
check_columns(student_test, student_columns)
check_columns(resource_train, resource_columns)
check_columns(resource_test, resource_columns)

students_train <- student_train
students_test <- student_test

resources_train <- resource_train
resources_test <- resource_test

students_train$Year <- as.integer(substr(students_train$Semester, 1, 4))
students_test$Year <- as.integer(substr(students_test$Semester, 1, 4))

resources_train$Year <- as.integer(substr(resources_train$Semester, 1, 4))
resources_test$Year <- as.integer(substr(resources_test$Semester, 1, 4))

students <- rbind(students_train, students_test)
resources <- rbind(resources_train, resources_test)

if (anyNA(students) || anyNA(resources)) {
  stop("Missing values found in datasets.")
}

if (anyDuplicated(students$Student_ID)) {
  stop("Duplicate student IDs found.")
}

if (anyDuplicated(resources[c("Semester", "Elective_Name")])) {
  stop("Duplicate semester-elective resource records found.")
}

valid_preferences <- apply(
  students[, prefs, drop = FALSE],
  1,
  function(x) length(unique(x)) == 8
)

if (!all(valid_preferences)) {
  stop("Some students have duplicate or incomplete preferences.")
}

if (any(students$CGPA < 0 | students$CGPA > 10)) {
  stop("Invalid CGPA values found.")
}

valid_allocation <- vapply(seq_len(nrow(students)), function(i) {
  students$Allocated_Elective[i] %in%
    unlist(students[i, prefs], use.names = FALSE)
}, logical(1))

if (!all(valid_allocation)) {
  stop("Some allocated electives are not in student preferences.")
}

electives <- sort(unique(as.character(unlist(
  students_train[, prefs, drop = FALSE],
  use.names = FALSE
))))

if (!setequal(electives, resources$Elective_Name)) {
  stop("Student and resource elective names do not match.")
}

cat("Data validation successful.\n")


# 3. Calculate weighted elective demand

preference_long <- do.call(rbind, lapply(1:8, function(i) {
  data.frame(
    Semester = students$Semester,
    Year = students$Year,
    Elective = students[[prefs[i]]],
    Rank = i,
    Weight = 9 - i
  )
}))

preference_long$P1_Count <- as.integer(preference_long$Rank == 1)
preference_long$Top4_Count <- as.integer(preference_long$Rank <= 4)

demand <- aggregate(
  cbind(Weight, P1_Count, Top4_Count) ~ Year + Elective,
  data = preference_long,
  FUN = sum
)

names(demand)[names(demand) == "Weight"] <- "Weighted_Demand"

table1 <- reshape(
  demand[, c("Year", "Elective", "Weighted_Demand")],
  idvar = "Elective",
  timevar = "Year",
  direction = "wide"
)


# 4. Calculate student satisfaction

get_rank <- function(allocation, preferences) {
  r <- match(allocation, preferences)
  if (is.na(r)) 0L else as.integer(r)
}

students$Satisfaction_Rank <- vapply(seq_len(nrow(students)), function(i) {
  get_rank(
    students$Allocated_Elective[i],
    unlist(students[i, prefs], use.names = FALSE)
  )
}, integer(1))

students$Satisfaction <- ifelse(
  students$Satisfaction_Rank == 1, "Excellent",
  ifelse(
    students$Satisfaction_Rank == 2, "Very Good",
    ifelse(
      students$Satisfaction_Rank == 3, "Good",
      ifelse(
        students$Satisfaction_Rank == 4, "Acceptable",
        ifelse(
          students$Satisfaction_Rank >= 5,
          "Low Satisfaction",
          "Not Allocated"
        )
      )
    )
  )
)

satisfaction <- aggregate(
  cbind(Total = rep(1, nrow(students)),
        Top4 = as.integer(students$Satisfaction_Rank %in% 1:4)),
  by = list(Year = students$Year),
  FUN = sum
)

satisfaction$Satisfaction_Percentage <- round(
  100 * satisfaction$Top4 / satisfaction$Total, 2
)

table2 <- satisfaction


# 5. Table 3: Existing allocation and capacity

allocation_counts <- aggregate(
  list(Students_Allocated = rep(1, nrow(students))),
  by = list(
    Semester = students$Semester,
    Elective_Name = students$Allocated_Elective
  ),
  FUN = sum
)

capacity <- resources

capacity$Section_Capacity <- pmin(
  capacity$Classroom_Capacity_Per_Section,
  capacity$Faculty_Capacity_Per_Section
)

capacity$Current_Feasible_Capacity <- pmin(
  capacity$Current_Seats,
  capacity$Existing_Sections * capacity$Section_Capacity
)

capacity$Maximum_Feasible_Capacity <-
  capacity$Faculty_Available * capacity$Section_Capacity

table3 <- merge(
  capacity,
  allocation_counts,
  by = c("Semester", "Elective_Name"),
  all.x = TRUE
)

table3$Students_Allocated[
  is.na(table3$Students_Allocated)
] <- 0

table3$Unused_Capacity <- pmax(
  0,
  table3$Current_Feasible_Capacity -
    table3$Students_Allocated
)


# 6. Phase 4: Train demand prediction model

train_counts <- aggregate(
  list(Enrollment = rep(1, nrow(students_train))),
  by = list(
    Year = students_train$Year,
    Elective = students_train$Allocated_Elective
  ),
  FUN = sum
)

training_grid <- expand.grid(
  Year = sort(unique(students_train$Year)),
  Elective = electives,
  stringsAsFactors = FALSE
)

train_counts <- merge(
  training_grid,
  train_counts,
  by = c("Year", "Elective"),
  all.x = TRUE
)

train_counts$Enrollment[
  is.na(train_counts$Enrollment)
] <- 0

models <- lapply(electives, function(e) {
  model_data <- train_counts[train_counts$Elective == e, ]
  lm(Enrollment ~ Year, data = model_data)
})

names(models) <- electives

predictions <- data.frame(
  Elective = electives,
  Predicted_Enrollment = sapply(electives, function(e) {
    value <- predict(
      models[[e]],
      newdata = data.frame(Year = 2026)
    )
    max(0, round(value))
  })
)

actual_2026 <- aggregate(
  list(Actual_Enrollment = rep(1, nrow(students_test))),
  by = list(Elective = students_test$Allocated_Elective),
  FUN = sum
)

prediction_results <- merge(
  predictions,
  actual_2026,
  by = "Elective",
  all.x = TRUE
)

prediction_results$Actual_Enrollment[
  is.na(prediction_results$Actual_Enrollment)
] <- 0

prediction_results$Error <-
  prediction_results$Actual_Enrollment -
  prediction_results$Predicted_Enrollment

MAE <- mean(abs(prediction_results$Error))

RMSE <- sqrt(mean(prediction_results$Error^2))

cat("Prediction MAE:", round(MAE, 2), "\n")
cat("Prediction RMSE:", round(RMSE, 2), "\n")


# 7. Phase 5: Resource recommendations

recommendations <- merge(
  predictions,
  resources_test,
  by.x = "Elective",
  by.y = "Elective_Name"
)

recommendations$Section_Capacity <- pmin(
  recommendations$Classroom_Capacity_Per_Section,
  recommendations$Faculty_Capacity_Per_Section
)

recommendations$Maximum_Feasible_Capacity <-
  recommendations$Faculty_Available *
  recommendations$Section_Capacity

recommendations$Recommended_Seats <- pmin(
  recommendations$Predicted_Enrollment,
  recommendations$Maximum_Feasible_Capacity
)

recommendations$Recommended_Sections <- ifelse(
  recommendations$Recommended_Seats == 0,
  0,
  ceiling(
    recommendations$Recommended_Seats /
      recommendations$Section_Capacity
  )
)

recommendations$Unmet_Demand <- pmax(
  0,
  recommendations$Predicted_Enrollment -
    recommendations$Maximum_Feasible_Capacity
)

table4 <- recommendations[, c(
  "Elective",
  "Predicted_Enrollment",
  "Current_Seats",
  "Recommended_Seats",
  "Maximum_Feasible_Capacity",
  "Recommended_Sections",
  "Unmet_Demand"
)]


# 8. Student allocation using preference and CGPA

allocate_students <- function(data, recommendations) {
  
  result <- data
  result$New_Allocation <- NA_character_
  
  seat_limits <- setNames(
    recommendations$Recommended_Seats,
    recommendations$Elective
  )
  
  for (rank in 1:8) {
    
    pref_col <- paste0("P", rank)
    
    pending <- which(is.na(result$New_Allocation))
    
    if (length(pending) == 0) break
    
    choices <- unique(result[[pref_col]][pending])
    
    for (elective in choices) {
      
      if (is.na(elective) || !elective %in% names(seat_limits)) {
        next
      }
      
      used <- sum(
        result$New_Allocation == elective,
        na.rm = TRUE
      )
      
      available <- max(0, seat_limits[[elective]] - used)
      
      if (available == 0) next
      
      candidates <- pending[
        result[[pref_col]][pending] == elective
      ]
      
      candidates <- candidates[
        order(-result$CGPA[candidates],
              result$Student_ID[candidates])
      ]
      
      selected <- head(candidates, available)
      
      result$New_Allocation[selected] <- elective
    }
  }
  
  result$New_Satisfaction_Rank <- vapply(
    seq_len(nrow(result)),
    function(i) {
      get_rank(
        result$New_Allocation[i],
        unlist(result[i, prefs], use.names = FALSE)
      )
    },
    integer(1)
  )
  
  result
}

optimized_allocation <- allocate_students(
  students_test,
  recommendations
)

existing_summary <- data.frame(
  Method = "Existing Allocation",
  Allocated = sum(students_test$Satisfaction_Rank > 0),
  Top4 = sum(students_test$Satisfaction_Rank %in% 1:4)
)

new_summary <- data.frame(
  Method = "Optimized Allocation",
  Allocated = sum(optimized_allocation$New_Satisfaction_Rank > 0),
  Top4 = sum(optimized_allocation$New_Satisfaction_Rank %in% 1:4)
)

comparison <- rbind(existing_summary, new_summary)

comparison$Satisfaction_Percentage <- round(
  100 * comparison$Top4 / nrow(students_test), 2
)

comparison$Unallocated <- nrow(students_test) -
  comparison$Allocated


# 9. Visualization 1: Year-wise elective demand

plot1 <- ggplot(
  demand,
  aes(
    x = Year,
    y = Weighted_Demand,
    color = Elective,
    group = Elective
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2022:2025) +
  labs(
    title = "Year-wise Elective Demand Trends",
    x = "Year",
    y = "Weighted Demand Score",
    color = "Elective"
  ) +
  theme_minimal()

print(plot1)


# 10. Visualization 2: Student satisfaction

plot2 <- ggplot(
  satisfaction,
  aes(x = factor(Year), y = Satisfaction_Percentage)
) +
  geom_col(fill = "steelblue", width = 0.65) +
  geom_text(
    aes(label = paste0(Satisfaction_Percentage, "%")),
    vjust = -0.4
  ) +
  scale_y_continuous(limits = c(0, 110)) +
  labs(
    title = "Student Satisfaction by Year",
    x = "Year",
    y = "Students Receiving Top 4 Preferences (%)"
  ) +
  theme_minimal()

print(plot2)


# 11. Visualization 3: Allocation vs capacity

plot3_data <- rbind(
  data.frame(
    Elective = table3$Elective_Name[table3$Semester == "2026-Odd"],
    Measure = "Students Allocated",
    Students = table3$Students_Allocated[table3$Semester == "2026-Odd"]
  ),
  data.frame(
    Elective = table3$Elective_Name[table3$Semester == "2026-Odd"],
    Measure = "Current Feasible Capacity",
    Students = table3$Current_Feasible_Capacity[table3$Semester == "2026-Odd"]
  ),
  data.frame(
    Elective = table3$Elective_Name[table3$Semester == "2026-Odd"],
    Measure = "Maximum Feasible Capacity",
    Students = table3$Maximum_Feasible_Capacity[table3$Semester == "2026-Odd"]
  )
)

plot3 <- ggplot(
  plot3_data,
  aes(x = Elective, y = Students, fill = Measure)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "2026 Allocation vs Available Capacity",
    x = "Elective",
    y = "Number of Students"
  ) +
  theme_minimal()

print(plot3)


# 12. Visualization 4: Prediction accuracy

plot4_data <- rbind(
  data.frame(
    Elective = prediction_results$Elective,
    Measure = "Predicted",
    Students = prediction_results$Predicted_Enrollment
  ),
  data.frame(
    Elective = prediction_results$Elective,
    Measure = "Actual",
    Students = prediction_results$Actual_Enrollment
  )
)

plot4 <- ggplot(
  plot4_data,
  aes(x = Elective, y = Students, fill = Measure)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "2026 Predicted vs Actual Enrollment",
    x = "Elective",
    y = "Students"
  ) +
  theme_minimal()

print(plot4)


# 13. Visualization 5: Recommended seats and sections

plot5_data <- rbind(
  data.frame(
    Elective = recommendations$Elective,
    Measure = "Predicted Demand",
    Students = recommendations$Predicted_Enrollment
  ),
  data.frame(
    Elective = recommendations$Elective,
    Measure = "Recommended Seats",
    Students = recommendations$Recommended_Seats
  ),
  data.frame(
    Elective = recommendations$Elective,
    Measure = "Maximum Capacity",
    Students = recommendations$Maximum_Feasible_Capacity
  )
)

plot5 <- ggplot(
  plot5_data,
  aes(x = Elective, y = Students, fill = Measure)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Demand vs Recommended Seats vs Maximum Capacity",
    x = "Elective",
    y = "Number of Students"
  ) +
  theme_minimal()

print(plot5)


# 14. Visualization 6: Satisfaction comparison

plot6 <- ggplot(
  comparison,
  aes(x = Method, y = Satisfaction_Percentage, fill = Method)
) +
  geom_col(width = 0.55) +
  geom_text(
    aes(label = paste0(Satisfaction_Percentage, "%")),
    vjust = -0.4
  ) +
  scale_y_continuous(limits = c(0, 110)) +
  labs(
    title = "Existing vs Optimized Student Satisfaction",
    x = "Allocation Method",
    y = "Top 4 Satisfaction (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(plot6)


# 15. Save tables and plots

dir.create("outputs", showWarnings = FALSE)

write.csv(table1, "outputs/Table1_Demand.csv", row.names = FALSE)
write.csv(table2, "outputs/Table2_Satisfaction.csv", row.names = FALSE)
write.csv(table3, "outputs/Table3_Capacity.csv", row.names = FALSE)
write.csv(table4, "outputs/Table4_Recommendations.csv", row.names = FALSE)

write.csv(
  prediction_results,
  "outputs/Prediction_Results.csv",
  row.names = FALSE
)

write.csv(
  optimized_allocation,
  "outputs/Optimized_Allocation.csv",
  row.names = FALSE
)

write.csv(
  comparison,
  "outputs/Satisfaction_Comparison.csv",
  row.names = FALSE
)

ggsave("outputs/Plot1_Demand.png", plot1, width = 11, height = 7)
ggsave("outputs/Plot2_Satisfaction.png", plot2, width = 9, height = 5)
ggsave("outputs/Plot3_Capacity.png", plot3, width = 11, height = 7)
ggsave("outputs/Plot4_Prediction.png", plot4, width = 11, height = 7)
ggsave("outputs/Plot5_Recommendations.png", plot5, width = 11, height = 7)
ggsave("outputs/Plot6_Comparison.png", plot6, width = 9, height = 5)

cat("\nAll five phases executed.\n")
cat("Six plots and all result tables saved in the outputs folder.\n")
cat("MAE:", round(MAE, 2), "\n")
cat("RMSE:", round(RMSE, 2), "\n")
