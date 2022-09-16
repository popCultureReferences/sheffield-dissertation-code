`%>%` <- magrittr::`%>%`
# read in run data and predictor/predictant/reaction names
run_data <- readr::read_csv("wave3_train.csv")%>%
  tidyr::drop_na() %>%
  dplyr::bind_rows(
    readr::read_csv("wave3_1_training_runs.csv") )
required_names <- readr::read_rds(
  "~/Maths/Sheffield/dissertation/data/required_names.rds")
observations <- readr::read_rds("~/Maths/Sheffield/dissertation/data/expDt.rda")
reactions <- required_names$reactions
predictors <- required_names$predictors
predictants <- required_names$predictants
predictants <-
# All the predictants are in the set of observed energies, but worth a check
predictants_subset <- predictants[predictants %in% observations$L1]
models <- parallel::mclapply(reactions, function(reaction){

  reaction_data <- run_data %>%
    dplyr::filter(Reaction == reaction)
  # NB CHANGED SCALING OF BOTH INPUTS AND OUTPUTS
  # pull out predictors and scale back to [-1,+1]
  design_matrix <- reaction_data %>%
    dplyr::select(predictors)
  design_matrix <- log(design_matrix, base=10)
  responses <- reaction_data %>%
    dplyr::select(predictants)
  # iterate over each column in the predictant data
  purrr::imap(responses, ~{
    logging::loginfo("Training model for Reaction: %s Energy: %s MeV", reaction, .y)
    tryCatch(
    # Allow nugget estim
    {  DiceKriging::km(design = design_matrix, response = .x,
                      formula = as.formula(~., ),
                      nugget.estim = T) },
      error = function(e){
       logging::loginfo("Model failure: %s %s", reaction, .y)
        NA  } )

  })
},mc.cores=7 )
names(models) <- reactions
saveRDS(models, "dice_models_wave3.rds")


