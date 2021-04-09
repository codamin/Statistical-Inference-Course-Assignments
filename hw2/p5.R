################################## CALCULATION #################################
cat('part one calculation percentage:', dbinom(0, 100, 0.005) * 100, '%\n')

cat('part two simulation percentage:', (1 - dbinom(0, 100, 0.005) - dbinom(1, 100, 0.005)) * 100, '%\n')


################################## SIMULATION #################################
create_one_box <- function() {
  boxes = list(sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.005, 0.995)));
  return(boxes)
}
num_boxes <- 1000;

create_boxes <- function() {
  boxes <- c()
  for (i in 1:num_boxes)
    boxes <-c(boxes, create_one_box())
  return(boxes)
}

#### PART a : Finding percentage of boxes which have no damaged pill
num_epoches <- 100

all_healthy_percentages <- c()
for (i in 1:num_epoches) {
  boxes <- create_boxes()
  num_healthy_boxes = sum(sapply(boxes, function (x) sum(unlist(x)) == 100))
  percentage = num_healthy_boxes / num_boxes * 100
  all_healthy_percentages <- c(all_healthy_percentages, percentage)
}

cat('part one simulation percentage:', mean(all_healthy_percentages), '%\n')


atleast_two_damaged_percentages <- c()
for (i in 1:num_epoches) {
  boxes <- create_boxes()
  num_healthy_boxes = sum(sapply(boxes, function (x) sum(unlist(x)) <= 98))
  percentage = num_healthy_boxes / num_boxes * 100
  atleast_two_damaged_percentages <- c(atleast_two_damaged_percentages, percentage)
}

cat('part two simulation percentage:', mean(atleast_two_damaged_percentages), '%\n')





