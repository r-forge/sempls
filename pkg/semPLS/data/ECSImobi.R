load("mobi.rda")
load("ECSIsm.rda")
load("ECSImm.rda")
ECSI <- semPLS:::specifyPLSM(data=mobi, strucmod=ECSIsm, measuremod=ECSImm, order="generic")

