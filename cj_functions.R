#' Compute inter-rater reliability for CJ decisions
#'
#' @param decisions_data Table of CJ decisions; each row is a judgement, and there are columns specified in the following parameters
#' @param chosen_col The column in decisions_data that holds the winner of each judgement
#' @param notchosen_col The column in decisions_data that holds the loser of each judgement
#' @param judge_col The column in decisions_data that holds the judge identifier
#' @param judge_group_size Either NULL (meaning split the judges into halves) or an integer specifying the number of judges to put in each subgroup
#' @return nested data frame, containing details of the two randomly-selected judging subgroups, and the correlation between the scales produced by each group
#' @import sirt
#' @import dplyr
split_halves_irr <- function(decisions_data,
                             chosen_col = "Won",
                             notchosen_col = "Lost",
                             judge_col = "JudgeID",
                             judge_group_size = NULL
) {
  decisions_for_sirt <- decisions_data %>%
    dplyr::select(
      JudgeID = all_of(judge_col),
      chosen = all_of(chosen_col),
      notChosen = all_of(notchosen_col)
    ) %>%
    mutate(winner = 1) %>% # indicate to sirt::btm that the winner is the leftmost column
    data.frame
  
  # Create the judging groups - either as a split half, or with specified size
  if(is.null(judge_group_size)) {
    # Group 1: sample half of the judges at random
    judge_group1 <- decisions_for_sirt %>% 
      select(JudgeID) %>% 
      distinct() %>% 
      slice_sample(prop = 0.5)
    # Group 2: the remaining judges
    judge_group2 <- decisions_for_sirt %>% 
      select(JudgeID) %>% 
      distinct() %>% 
      anti_join(judge_group1, by = c("JudgeID"))
  } else {
    num_judges = decisions_for_sirt %>%
      select(JudgeID) %>%
      distinct() %>% nrow()
    if (2 * judge_group_size > num_judges) {
      stop(paste0("Judge group size ", judge_group_size, " not possible - there are only ", num_judges, " judges"))
    }
    # Group 1: sample the required number of judges
    judge_group1 <- decisions_for_sirt %>% 
      select(JudgeID) %>% 
      distinct() %>% 
      slice_sample(n = judge_group_size)
    # Group 2: sample the required number of judges from the remaining judges
    judge_group2 <- decisions_for_sirt %>% 
      select(JudgeID) %>% 
      distinct() %>% 
      anti_join(judge_group1, by = c("JudgeID")) %>% 
      slice_sample(n = judge_group_size)
  }
  
  judgements1 <- decisions_for_sirt %>% semi_join(judge_group1, by = c("JudgeID")) %>% select(-JudgeID)
  judgements2 <- decisions_for_sirt %>% semi_join(judge_group2, by = c("JudgeID")) %>% select(-JudgeID)
  
  btm1 <- sirt::btm(judgements1 %>% data.frame , maxit=400 , fix.eta=0 , ignore.ties=TRUE )
  btm2 <- sirt::btm(judgements2 %>% data.frame , maxit=400 , fix.eta=0 , ignore.ties=TRUE )
  
  merged_effects <- merge(btm1$effects, btm2$effects, by="individual")
  
  # This is a simpler version which just returns the correlation coefficient:
  #return(cor(merged_effects$theta.x, merged_effects$theta.y, method="pearson"))
  return(tibble(
    judges1 = list(judge_group1),
    judges2 = list(judge_group2),
    # record the number of comparisons in each judge group
    nc1 = nrow(judgements1),
    nc2 = nrow(judgements2),
    cor = cor(merged_effects$theta.x, merged_effects$theta.y, method="pearson")
  ))
}

# This is a wrapper function that can be used to iterate the split halves process many times
split_halves_irr_iterations <- function(decisions_data,
                                        chosen_col = "Won",
                                        notchosen_col = "Lost",
                                        judge_col = "JudgeID",
                                        judge_group_size = NULL,
                                        iters = c(1:10)) {
  iterations <- tibble(iteration = iters) %>%
    group_by(iteration) %>%
    mutate(
      split_half_corr = list(split_halves_irr(
        decisions_data,
        chosen_col,
        notchosen_col,
        judge_col,
        judge_group_size
      ))
    )
  
  return(iterations)
}


#' Fit Bradley-Terry model to CJ data
#'
#' @param decisions_data Table of CJ decisions; each row is a judgement, and there are columns specified in the following parameters
#' @param chosen_col The column in decisions_data that holds the winner of each judgement
#' @param notchosen_col The column in decisions_data that holds the loser of each judgement
#' @param judge_col The column in decisions_data that holds the judge identifier
#' @param split_halves_iterations How many iterations to use for split-halves reliability
#' @return data frame with the sirt::btm output in tidy form
#' @import sirt
#' @import dplyr
btm_for_cj <- function(decisions_data, chosen_col = "Won", notchosen_col = "Lost", judge_col = "JudgeID", split_halves_iterations = 10) {
  # prepare the judgement data in the form required by sirt::btm
  decisions_for_sirt <- decisions_data %>%
    select(chosen = all_of(chosen_col), notChosen = all_of(notchosen_col)) %>%
    mutate(winner = 1) %>% # indicate to sirt::btm that the winner is the leftmost column
    data.frame
  
  # fit the Bradley-Terry model
  mdl <- sirt::btm(decisions_for_sirt,
                   maxit=400,
                   fix.eta=0, # the "home advantage" should be 0 as left column does not mean home
                   ignore.ties=TRUE)
  
  # find the judge infit values
  # TODO - reconcile the approach used in nmmBtm::btm_fit_2 with the one in sirtmarkdown.Rmd that gives different results
  judge_fits <-
    nmmBtm::btm_fit_2(
      mdl[["probs"]],
      decisions_data %>% rename(judgeName = all_of(judge_col))
    ) %>%
    mutate(
      infit_mean = mean(infit),
      infit_sd = sd(infit),
      # note which judges have an infit value more than 2sd above the mean
      discard = infit > infit_mean + 2*infit_sd
    ) %>% 
    select(judgeName, contains("infit"), discard)
  
  # Script misfits
  # TODO - bring this in from sirtmarkdown.Rmd
  #      - or, is it already there in the btm_estimates? (infit/outfit cols)
  
  # Find the inter-rater reliability
  # TODO - probably just get rid of this and leave it to users to add on?
  #split_halves_iterations <-
  #  split_halves_irr_iterations(decisions_data, iters = c(1:split_halves_iterations))
  
  return(tibble(
    N_R = nrow(mdl[["effects"]]),
    N_A = nrow(judge_fits),
    N_C = nrow(decisions_for_sirt),
    ssr = list(mdl$mle.rel),
    btm_estimates = list(as_tibble(mdl[["effects"]])),
    #  split_halves = list(as_tibble(split_halves_iterations)),
    judge_fits = list(as_tibble(judge_fits)),
    #num_split_halves = nrow(split_halves_iterations),
  ))
  
}