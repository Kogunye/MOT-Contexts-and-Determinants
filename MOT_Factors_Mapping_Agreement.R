install.packages("irr")
install.packages("lattice")
library(irr)
library(lattice)


#MOT Constraints by Coder 1 and Coder 2#
MOT_Constraints <- data.frame(
  Coder1 = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "EN", "PA", "AC", "COM"),
  Coder2 = c("AR", "AR", "CS", "DE", "DE", "CA", "AD", "SC", "LC", "PL", "PL", "TE", "PA", "PA", "COM")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_Constraints <- agree(MOT_Constraints)
print(Agreement_Percentage_MOT_Constraints)
Po_MOT_Constraints <- Agreement_Percentage_MOT_Constraints$value / 100

#Confusion matrix table#
conf_matrix_MOT_Constraints <- table(MOT_Constraints$Coder1, MOT_Constraints$Coder2)
print(conf_matrix_MOT_Constraints)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "darkred"))(100)
MOT_Constraints_Heatmap <- levelplot(conf_matrix_MOT_Constraints, col.regions=my_colors, xlab = "Coder1", ylab = "Coder2", 
                                     main= list(label = "MOT Constraints Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_Constraints), y = col(conf_matrix_MOT_Constraints), labels = conf_matrix_MOT_Constraints,  cex = 0.5, col="white")})
print(MOT_Constraints_Heatmap)

#Cohen's Kappa#
kappa_MOT_Constraints <- kappa2 (MOT_Constraints, weight = "unweighted")
print(kappa_MOT_Constraints)


#MOT Enablers by Coder 1 and Coder 2#
MOT_Enablers <- data.frame(
  Coder1 = c("AR", "AA", "AD", "CA", "PL", "EN", "AKI", "RC", "AC", "LC", "LC", "DE", "DE", "DE", "PL"),
  Coder2 = c("AR", "AD", "AD", "CA", "PL", "EN", "AKI", "RC", "PL", "LC", "LC", "DE", "DE", "DE", "PL")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_Enablers <- agree(MOT_Enablers)
print(Agreement_Percentage_MOT_Enablers)
Po_MOT_Enablers <- Agreement_Percentage_MOT_Enablers$value / 100

#Confusion matrix table#
conf_matrix_MOT_Enablers <- table(MOT_Enablers$Coder1, MOT_Enablers$Coder2)
print(conf_matrix_MOT_Enablers)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "darkblue"))(100)
MOT_Enablers_Heatmap <- levelplot(conf_matrix_MOT_Enablers, col.regions=my_colors, xlab = "Coder1", ylab = "Coder2", 
                                  main= list(label = "MOT Enablers Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_Enablers), y = col(conf_matrix_MOT_Enablers), labels = conf_matrix_MOT_Enablers,  cex = 0.5, col="white")})
print(MOT_Enablers_Heatmap)

#Cohen's Kappa#
kappa_MOT_Enablers <- kappa2 (MOT_Enablers, weight = "unweighted")
print(kappa_MOT_Enablers)


#MOT Constraints and Enablers by Coder 1 and Coder 2#
MOT_ConstraintsandEnablers <- data.frame(
  Coder1 = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "EN", "PA", "AC", "COM", "AR", "AA", "AD", "CA", "PL", "EN", "AKI", "RC", "AC", "LC", "LC", "DE", "DE", "DE", "PL"),
  Coder2 = c("AR", "AR", "CS", "DE", "DE", "CA", "AD", "SC", "LC", "PL", "PL", "TE", "PA", "PA", "COM", "AR", "AD", "AD", "CA", "PL", "EN", "AKI", "RC", "PL", "LC", "LC", "DE", "DE", "DE", "PL")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_ConstraintsandEnablers <- agree(MOT_ConstraintsandEnablers)
print(Agreement_Percentage_MOT_ConstraintsandEnablers)
Po_MOT_ConstraintsandEnablers <- Agreement_Percentage_MOT_ConstraintsandEnablers$value / 100

#Confusion matrix table#
conf_matrix_MOT_ConstraintsandEnablers <- table(MOT_ConstraintsandEnablers$Coder1, MOT_ConstraintsandEnablers$Coder2)
print(conf_matrix_MOT_ConstraintsandEnablers)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "purple4"))(100)
MOT_ConstraintsandEnablers_Heatmap <- levelplot(conf_matrix_MOT_ConstraintsandEnablers, col.regions=my_colors, xlab = "Coder1", ylab = "Coder2", 
                                                main= list(label = "MOT Constraints and Enablers Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_ConstraintsandEnablers), y = col(conf_matrix_MOT_ConstraintsandEnablers), labels = conf_matrix_MOT_ConstraintsandEnablers,  cex = 0.5, col="white")})
print(MOT_ConstraintsandEnablers_Heatmap)

#Cohen's Kappa#
kappa_MOT_ConstraintsandEnablers <- kappa2 (MOT_ConstraintsandEnablers, weight = "unweighted")
print(kappa_MOT_ConstraintsandEnablers)


#MOT Constraints 2 by Initial coding and Final coding#
MOT_Constraints2 <- data.frame(
  Initialcoding = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "TE", "PA", "AC", "COM"),
  Finalcoding = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "EN", "PA", "AN", "COM")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_Constraints2 <- agree(MOT_Constraints2)
print(Agreement_Percentage_MOT_Constraints2)
Po_MOT_Constraints2 <- Agreement_Percentage_MOT_Constraints2$value / 100

#Confusion matrix table#
conf_matrix_MOT_Constraints2 <- table(MOT_Constraints2$Initialcoding, MOT_Constraints2$Finalcoding)
print(conf_matrix_MOT_Constraints2)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "darkred"))(100)
MOT_Constraints2_Heatmap <- levelplot(conf_matrix_MOT_Constraints2, col.regions=my_colors, xlab = "Initialcoding", ylab = "Finalcoding", 
                                      main= list(label = "MOT Constraints 2 Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_Constraints2), y = col(conf_matrix_MOT_Constraints2), labels = conf_matrix_MOT_Constraints2,  cex = 0.5, col="white")})
print(MOT_Constraints2_Heatmap)

#Cohen's Kappa#
kappa_MOT_Constraints2 <- kappa2 (MOT_Constraints2, weight = "unweighted")
print(kappa_MOT_Constraints2)


#MOT Enablers 2 by Initial coding and Final coding#
MOT_Enablers2 <- data.frame(
  Initialcoding = c("AR", "AD", "AD", "CA", "PL", "EN", "AKI", "RC", "AC", "LC", "LC", "DE", "DE", "DE", "PL"),
  Finalcoding = c("AR", "AA", "AD", "CA", "PL", "EN", "AKI", "RC", "AN", "LC", "LC", "DE", "DE", "DE", "PL")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_Enablers2 <- agree(MOT_Enablers2)
print(Agreement_Percentage_MOT_Enablers2)
Po_MOT_Enablers2 <- Agreement_Percentage_MOT_Enablers2$value / 100

#Confusion matrix table#
conf_matrix_MOT_Enablers2 <- table(MOT_Enablers2$Initialcoding, MOT_Enablers2$Finalcoding)
print(conf_matrix_MOT_Enablers2)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "darkblue"))(100)
MOT_Enablers2_Heatmap <- levelplot(conf_matrix_MOT_Enablers2, col.regions=my_colors, xlab = "Initialcoding", ylab = "Finalcoding", 
                                   main= list(label = "MOT Enablers 2 Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_Enablers2), y = col(conf_matrix_MOT_Enablers2), labels = conf_matrix_MOT_Enablers2,  cex = 0.5, col="white")})
print(MOT_Enablers2_Heatmap)

#Cohen's Kappa#
kappa_MOT_Enablers2 <- kappa2 (MOT_Enablers2, weight = "unweighted")
print(kappa_MOT_Enablers2)


#MOT Constraints and Enablers 2 by Initial coding and Final coding#
MOT_ConstraintsandEnablers2 <- data.frame(
  Initialcoding = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "TE", "PA", "AC", "COM", "AR", "AD", "AD", "CA", "PL", "EN", "AKI", "RC", "AC", "LC", "LC", "DE", "DE", "DE", "PL"),
  Finalcoding = c("AR", "AR", "CS", "DE", "DE", "CA", "CO", "SC", "LC", "PL", "PL", "EN", "PA", "AN", "COM", "AR", "AA", "AD", "CA", "PL", "EN", "AKI", "RC", "AN", "LC", "LC", "DE", "DE", "DE", "PL")
)

#Percent agreement (Po)#
Agreement_Percentage_MOT_ConstraintsandEnablers2 <- agree(MOT_ConstraintsandEnablers2)
print(Agreement_Percentage_MOT_ConstraintsandEnablers2)
Po_MOT_ConstraintsandEnablers2 <- Agreement_Percentage_MOT_ConstraintsandEnablers2$value / 100

#Confusion matrix table#
conf_matrix_MOT_ConstraintsandEnablers2 <- table(MOT_ConstraintsandEnablers2$Initialcoding, MOT_ConstraintsandEnablers2$Finalcoding)
print(conf_matrix_MOT_ConstraintsandEnablers2)

#Confusion matrix heatmap#
my_colors <- colorRampPalette(c("grey", "purple4"))(100)
MOT_ConstraintsandEnablers2_Heatmap <- levelplot(conf_matrix_MOT_ConstraintsandEnablers2, col.regions=my_colors, xlab = "Initialcoding", ylab = "Finalcoding", 
                                                 main= list(label = "MOT Constraints and Enablers 2 Agreement Heatmap", cex = 1), aspect = "fill", panel = function(...) {panel.levelplot(...); panel.text( x = row(conf_matrix_MOT_ConstraintsandEnablers2), y = col(conf_matrix_MOT_ConstraintsandEnablers2), labels = conf_matrix_MOT_ConstraintsandEnablers2,  cex = 0.5, col="white")})
print(MOT_ConstraintsandEnablers2_Heatmap)

#Cohen's Kappa#
kappa_MOT_ConstraintsandEnablers2 <- kappa2 (MOT_ConstraintsandEnablers2, weight = "unweighted")
print(kappa_MOT_ConstraintsandEnablers2)