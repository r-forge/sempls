load("mobi.rda")
load("ECSIsm.rda")
load("ECSImm.rda")
ECSImobi <- semPLS:::specifyPLSM(data=mobi, strucmod=ECSIsm, measuremod=ECSImm, order="generic")

