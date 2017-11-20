source('autoload.R', chdir=T)




fullmodel <- lm(DEXAtotyag ~ yas + boy + kilo + cev_biceps + 
                  cev_kalca + cev_boyun + cev_bel + 
                  cev_uyluk + cev_baldir + cev_elblegi + 
                  cev_kulacuzun + dkk_biceps + dkk_triceps +
                  dkk_sscapula + dkk_silliak + dkk_abdomen +
                  dkk_quadriceps + dkk_gogus, data=genderDb$`1`)




step(fullmodel, direction = "backward", trace=FALSE ) 





























rf_groups <- lapply(genderDb, function(gdb){ 
  fullmodel <- lm(DEXAtotyag ~ yas + boy + kilo + cev_biceps + 
                    cev_kalca + cev_boyun + cev_bel + 
                    cev_uyluk + cev_baldir + cev_elblegi + 
                    cev_kulacuzun + dkk_biceps + dkk_triceps +
                    dkk_sscapula + dkk_silliak + dkk_abdomen +
                    dkk_quadriceps + dkk_gogus, data=gdb)
  step(fullmodel, direction = "backward", trace=FALSE ) 
})
rf_groups
final <- rf_ga3$ga$final # Get features selected by GA
print(final)

save(rf_groups, file='rfGA30-100.RData')
