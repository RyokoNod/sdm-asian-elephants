
plot_shinystan_posteriors <- function(p, renamed_params, title){
  
  p$data$params <- renamed_params
  
  ggplot(data = p$data) +
    geom_segment(aes(x = ll, y = params, xend = hh, yend = params)) +
    geom_segment(aes(x = l, y = params, xend = h, yend = params), size=2, color="#D55E00") +
    geom_point(aes(x=m, y=params),shape=21, size=3, stroke=0.5, color="#D55E00", fill = "#56B4E9") +
    labs(title=title) +
    ylab("Parameters") + xlab("")
}


shinystan_output_folder <- "./bayesian_linearGLM/adjusted_priors/"
shinystan_output_file <- "bayeslinGLM_randCVfeat_posteriors_adjpriors.RData"

load(paste(shinystan_output_folder, shinystan_output_file, sep=""))

plot_title <- "Raw random CV features/ adjusted priors"

#renamed_params <- c("Intercept", "Coeff BIO08",
#                  "Coeff TXX", "Coeff BIO02",
#                  "Coeff TN90P", "Coeff ID",
#                  "Coeff BIO14", "Coeff BIO18",
#                  "Coeff CWD", "Coeff RX1DAY",
#                  "Coeff WSDI")
renamed_params <- c("Intercept", "Coeff BIO03",
                    "Coeff TN10P", "Coeff GSL",
                    "Coeff TNX", "Coeff ID",
                    "Coeff BIO14", "Coeff BIO18",
                    "Coeff CWD", "Coeff RX1DAY",
                    "Coeff WSDI")

plot_shinystan_posteriors(shinystan_multiparam_gg, renamed_params, plot_title)






