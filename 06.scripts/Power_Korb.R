# Korb 2015 Gender diff

# effect size over S1

# t-test = t(16) = -2.7
n = 7    # nb participant
t = -2.7  # t score



# Formula 1 paired t-test
# Since Sqrt(n(mean)) / sd = t
# Cohen's d = t/Sqrt(n) = (mean-constant)/sd 

d = t/sqrt(n)
# d = 0.654




# Formula 2 based on mean and SD
nb = 17
mean_vtx = 4.66
mean_S1 =  4.62
sd_vtx = 0.51
sd_S1 =  0.36
  
sd_pooled = sqrt( ((nb-1*sd_vtx^2)+(nb-1*sd_S1^2)) /  (nb+nb-2) )

d = (mean_vtx - mean_S1) / sd_pooled



# t-test = t(16) = -2.7
n = 17    # nb participant
t = -2.9  # t score



# Formula 1 paired t-test
# Since Sqrt(n(mean)) / sd = t
# Cohen's d = t/Sqrt(n) = (mean-constant)/sd 

d = t/sqrt(n)
