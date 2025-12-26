# Overview

This is a mini-research paper I wrote for my transportation economics class. It looks at the 
correlation between changes gas prices and voter behavior, specifically voter turnout and the vote 
share the incumbent receives. Note that I am looking at changes in gas prices from year to year,
not the level of gas prices themselves. 

# Method

I have county level data for all variables except annual averaged gas prices, which are at the state level.
I use a first-differenced version of OLS with state fixed effects and year fixed effects.

# Findings

I find null results with respect to the effect of gas prices on voter turnout. I find that an 10 cent increase 
in the change in gas prices is associated with a 1 percentage point decrease in the vote share the incumbant receives.

<img width="283" height="208" alt="Screenshot 2025-12-25 at 10 22 02 PM" src="https://github.com/user-attachments/assets/d50c4a5f-5801-4103-b130-bc9c2191ac9a" />


# Robustness/Placebo checks

I run several robustness and placebo checks. The placebo checks all pass cleanly with the exception of predicting 
current voter turnout with future changes in gas prices. In this case, p = 0.0123, but the correlation between current changes in 
gas prices and future changes was -0.4, which would reasonably affect the placebo. In addition, 
the placebo dataset has less data than the main model.

Robustness checks for the main finding of the association between gas price changes and incumbent vote share all
show mostly significant p-values and coefficients that are very similar in magnitude. The only robustness check 
that did not yield p < 0.05 was still marginally significant at p = 0.062. This model used county fixed effects which 
is far more conservative than state fixed effects.

<img width="337" height="231" alt="Screenshot 2025-12-25 at 10 21 21 PM" src="https://github.com/user-attachments/assets/378d5ef7-010b-4615-b7dc-04edd3eaac2a" />
