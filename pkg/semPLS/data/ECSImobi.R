if(!exists("mobi")) load("mobi.rda")
if(!exists("ECSIsm")) load("ECSIsm.rda")
if(!exists("ECSImm")) load("ECSImm.rda")
ECSImobi <- semPLS:::specifyPLSM(data=mobi, strucmod=ECSIsm, measuremod=ECSImm, order="generic")

