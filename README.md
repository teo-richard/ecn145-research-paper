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

<img width="288" height="202" alt="Screenshot 2025-12-25 at 10 32 05 PM" src="https://github.com/user-attachments/assets/61c8e8af-cc19-43d2-bd16-7aba0a14cb6b" />

<img width="363" height="334" alt="Screenshot 2025-12-25 at 10 32 13 PM" src="https://github.com/user-attachments/assets/ed34d7e4-a174-4f3d-8297-d91b6cae0887" />


# Robustness/Placebo checks

I run several robustness and placebo checks. The placebo checks all pass cleanly with the exception of predicting 
current voter turnout with future changes in gas prices. In this case, p = 0.0123, but the correlation between current changes in 
gas prices and future changes was -0.4, which would reasonably affect the placebo. In addition, 
the placebo dataset has less data than the main model.

Robustness checks for the main finding of the association between gas price changes and incumbent vote share all
show mostly significant p-values and coefficients that are very similar in magnitude. The only robustness check 
that did not yield p < 0.05 was still marginally significant at p = 0.062. This model used county fixed effects which 
is far more conservative than state fixed effects.


<img width="339" height="240" alt="Screenshot 2025-12-25 at 10 32 23 PM" src="https://github.com/user-attachments/assets/40cfb0b3-9a14-4258-8942-2887f8d6eef9" />

# Learning Outcomes

I learned about the process of research in this project. I learned about how to formulate a question that can actually be answered—I started out with some more vague questions like relationships between transportation and connectivity. This is hard to answer so I had to figure out how to ask a question that is both interesting, practically relevant, and answered with publically available data. 

I also got practice with the more mundane parts of writing a research paper—using LaTeX, clearly explaining all the ideas bouncing around in my head, and figuring out how to visualize my findings in an easy to read manner.

I encountered some problems, like wrapping my head around what fixed effects mean in a first-differenced model, initial issues with my placebo tests, and deciding how to code certain variables. 

Finally, I had a lot of fun being able to put all my ideas and findings into a coherent paper. I like this paper and think it has very relevant implications to today's world. You can read the conclusion in the paper for a little more detail, but my findings have implications regarding how politicians think about the effect of policies on gas prices, the effects EVs might have on voter behavior, and how environmental reforms might influence voters and therefore politicians. 

In general, this paper definitely sparked an interest in transportation and sustainability. It also made me realize just how important politics is in economics—no matter how much I'd like to pretend politics doesn't exist, it is indeed true that it is a serious force in the economy and must not be disregarded. 
