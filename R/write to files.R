# for (i in 1:6){
#   fn <- sprintf("Rings/Ring%s/runfolder/trees.dat",i)
#   
#   # write.table(c("&bbmgs","g0 = 0","/"),fn, 
#   #             row.names=FALSE, sep="\t", quote=FALSE,append =TRUE,
#   #             col.names = FALSE)
#   
#   write.table(c("&SPECLIST","ISPECIES =  1","/"),fn, 
#               row.names=FALSE, sep="\t", quote=FALSE,append =TRUE,
#               col.names = FALSE)
#   
# }
# 
# 
# for (i in 1:6){
#   
#   fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
#   
#   # write.table(c("&bbmgs","g0 = 0","/"),fn, 
#   #             row.names=FALSE, sep="\t", quote=FALSE,append =TRUE,
#   #             col.names = FALSE)
# 
#   write.table(c("&SPECIES","NSPECIES =  1","/"),fn, 
#               row.names=FALSE, sep="\t", quote=FALSE,append =TRUE,
#               col.names = FALSE)
#   
# }
# 
# for (i in 1:6){
#   
#   fn <- sprintf("Rings/Ring%s/runfolder/trees.dat",i)
# 
#   replaceNameList("SPECLIST", fn, 
#                   vals=list(ISPECIES =  1)
#   )
#   
# }

# for (i in 1:6){
#   
#   fn <- sprintf("Rings/Ring%s/runfolder/confile.dat",i)
#   
#   write.table(c("&HISTO","BINSIZE =  200","/"),fn,
#               row.names=FALSE, sep="\t", quote=FALSE,append =TRUE,
#               col.names = FALSE)
# }
