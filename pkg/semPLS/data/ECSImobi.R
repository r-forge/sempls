load(system.file("mobi.rda", package="semPLS"))
load(system.file("ECSIsm.rda", package="semPLS"))
load(system.file("ECSImm.rda", package="semPLS"))
ECSImobi <- semPLS:::specifyPLSM(data=mobi, strucmod=ECSIsm, measuremod=ECSImm, order="generic")

