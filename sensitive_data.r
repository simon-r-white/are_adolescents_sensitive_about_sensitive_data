##
## Code for "Are adolescents sensitive about sensitive data?"
##
## Emma Soneson, Mina Fazel, Puneetha S. Goli, OxWell Study Team, and Simon R. White (2025). "Are adolescents sensitive about sensitive data?".
##
## Note: The data are not publicly available because of ethical and information governance restrictions
##


library("sjPlot")
library("scales")
library("data.table")
library("cyphr")
library("foreign")
library("ggsci")
library("RColorBrewer")
library("kSamples") ## 

VERSION <- "final" ## for filenames

## DEFINE FILE LOCATIONS
DIRS <- list()
DIRS$USER.SSHKEY <- "~/.ssh" ## Location of ssh-key to decrypt data (must not be on secure drive!)
DIRS$MOUNT <- "OxWell" ## Mount location of secure drive
DIRS$WITHIN <- "networks_of_care" ## Project folder

DIRS$DATA      <<- file.path( DIRS$MOUNT, DIRS$WITHIN, "data" )
DIRS$PROJECT   <<- file.path( DIRS$MOUNT, DIRS$WITHIN, "scripts" )
DIRS$OUTPUTS   <<- file.path( DIRS$MOUNT, DIRS$WITHIN, "outputs" )

##
## The following line decrypts the data-key (using your ssh-rsa keyphrase)
KEY <- cyphr::data_key(path_data=DIRS$DATA,path_user=DIRS$USER.SSHKEY)


## Using OxWell2023 Release 7 (2024-03-18)
DATA <- cyphr::decrypt_object(key=KEY, data=file.path(DIRS$DATA,"DATA2023.r07.limited.rds-cyphr") )
##


##
## DETERMINE CONCERN ITEMS
##
CONCERN.SET <- c("a","b","c","d","e")

##
## SELECT SUBSET
##
SUBSET <- copy(DATA[FAB.FLAGCONSENT=="PASS" & FAB.SURVEYCOMPLETED=="Yes" & VERSION!="PR",])

##
## Manipulating/adding columns to SUBSET and making LONG (ie melted) versions of data set
##

##
## Data R7 has not accounted for skip, manually fix here (corrected in Release 8)
##
for( LAB in CONCERN.SET ) {
    SUBSET[get(paste0("X2770",LAB))!="NoResponse",c(paste0("X2771",LAB,"V")):=get(paste0("X2771",LAB))]
    SUBSET[get(paste0("X2770",LAB))=="NoResponse",c(paste0("X2771",LAB,"V")):="No concern"]
}


## Release 7 fudge (to be fixed later)
if ( 1 ) {
    SUBSET[GENDER.broad=="Gender diverse" & X1020=="Female",.(X1020,X1021F,GENDER.Specific,GENDER.broad)]
    SUBSET[GENDER.broad=="Boy" & X1020=="Male",.(X1020,X1021F,GENDER.Specific,GENDER.broad)]
    SUBSET[GENDER.broad=="Gender diverse" & X1020=="Male",.(X1020,X1021F,GENDER.Specific,GENDER.broad)]
    SUBSET[X1020=="Other" & X1021F=="NULL",.(X1020,X1021F,GENDER.Specific,GENDER.broad)]

    SUBSET[,GENDER:=GENDER.broad]
    SUBSET[X1020=="Male",GENDER:="Boy"]
    SUBSET[X1020=="Female",GENDER:="Girl"]
    SUBSET[X1020=="Prefer not to say",GENDER:="Prefer not to say"]
    SUBSET[X1020=="Other" & X1021F=="NULL",GENDER:="Gender diverse"]
    SUBSET[X1020=="NoResponse",GENDER:="NoResponse"]
}
if ( 1 ) {
    SUBSET[,SEX:=GENDER]
    Sex.TRANSLATE <- rbindlist( list(
        data.table(old="Boy",new="Boy"),
        data.table(old="Girl",new="Girl"),
        data.table(old="Gender diverse",new="Gender diverse"),
        data.table(old=c("Prefer not to say","Unsure"),new="Prefer not to say"),
        data.table(old=c("Silly/malicious/not gender"),new=NA_character_)
    ))        
    SUBSET[ Sex.TRANSLATE, on=.(GENDER=old), SEX2:=i.new ]
}


SUBSET[,ETHNICONS:=ETHNICGROUPINGS]
SUBSET[ETHNICONS=="Arab",ETHNICONS:="Other ethnic group"]
SUBSET[,ETHNICONS:=droplevels(ETHNICONS)]
    

##
## Make LONG version of subset
##
SPAN <- melt.data.table(data=SUBSET[,.SD,.SDcols=c("RID","KEYSTAGE","SEX","SEX2","ETHNICONS",paste0("X2770",CONCERN.SET),paste0("X2771",CONCERN.SET,"V"))],
                        id.vars=c("RID","KEYSTAGE","SEX","SEX2","ETHNICONS") )
SPAN[,CONCERN:=substr(variable,6,6)]
SPAN[,Question:=c("0"="ConcernType","1"="Impact")[substr(variable,5,5)]]
LONG <- SPAN[ rbindlist(CONCERNS.LIST), on=.(CONCERN) ]
LONG[,CONCERN:=NULL]
LONG[,variable:=NULL]
    


##
## Analysis
##
if( 1 ) {


    COLOURS <- brewer.pal(n=5,name="BuGn")
    
    if( 1 ) {
        ##
        ## for abstract
        ##

        ## Counts/percentages of at least one concern
        ##
        LONG[Question=="ConcernType",.(Concern.Binary=c("No concerns","One to five concerns")[1+any(value=="Ticked (Yes)")]),by=.(RID)][,.N,by=.(Concern.Binary)][,PROP:=N/sum(N)][]

        TAB <- LONG[Question=="ConcernType",.(Concern.Count=sum(value=="Ticked (Yes)")),by=.(RID)][,.N,by=.(Concern.Count)][,PROP:=100*N/sum(N)][order(-1*Concern.Count)]
        TAB[,Cumulative.PROP:=cumsum(PROP)]
        write.csv( format(TAB,nsmall=1,digits=0), file=file.path(DIRS$PROJECT,sprintf("%s.cumulative.proportion.of.concerns.csv",VERSION)), row.names=FALSE )


        ## Concern--Accuracy cross-tabulation
        ##
        TAB <- LONG[,.(Concern.Total=sum((Question=="ConcernType" & value=="Ticked (Yes)")),Accuracy.Total=sum((Question=="Impact" & value=="Yes"))),by=.(RID)]

        TABalt <- LONG[,.(Concern.Total=sum((Question=="ConcernType" & value=="Ticked (Yes)")),Accuracy.Total=sum((Question=="Impact" & (value%in%c("Yes","Prefer not to say"))))),by=.(RID)]
        
        write.csv( TAB[,xtabs( ~ Concern.Total + Accuracy.Total ) ],
                  file=file.path(DIRS$PROJECT,sprintf("%s.Concern--Accuracy.xtabs.csv",VERSION)) )
    }
    

    if( 1 ) {
        ##
        ## H1 & H2 plot and table
        ##
        COUNTS <- dcast.data.table(data=LONG[Question=="Impact"],formula= SEX + KEYSTAGE + ETHNICONS + Concern ~ value, fun.aggregate=length )
        setnames(x=COUNTS,old="NoResponse",new="No response")
        setcolorder(x=COUNTS,neworder=c("SEX", "KEYSTAGE", "ETHNICONS", "Concern", "No concern","No response","Prefer not to say","No","Yes"))

        ##
        ## H1 <<Accuarcy concerns as basic plot+table>>

        
        H1.COUNTS <- COUNTS[,as.list(colSums(.SD)),by=.(Concern),.SDcols=c("No concern","No response","Prefer not to say","No","Yes")]


        write.csv( H1.COUNTS, file=file.path(DIRS$PROJECT,sprintf("%s.H1.counts.csv",VERSION)) )
        
        pdf(file=file.path(DIRS$PROJECT,sprintf("%s.H1.barplot.pdf",VERSION)),width=24/cm(1), height=16/cm(1))
        par(mar=c(6,5,4,2))
        
        BP.INPUT <- (as.matrix(H1.COUNTS[order(`No concern`)][,100*.SD[,-1]/rowSums(.SD),by=.(Concern)],rownames="Concern"))
        BP <- barplot(t(BP.INPUT),las=1, names.arg=rep("",NROW(BP.INPUT)),ylab="Percentage (%)",col=COLOURS[-1])
        mtext(text=sapply(strwrap(rownames(BP.INPUT),width=23,simplify=FALSE),FUN=paste0,collapse="\n"),at=BP,side=1,line=3,adj=0.5,cex=0.8)
        LEGEND <- colnames(BP.INPUT)
        LEGEND[which(LEGEND=="No")] <- "No (accurate responses)"
        LEGEND[which(LEGEND=="Yes")] <- "Yes (inaccurate responses)"
        legend("topright",legend=LEGEND,fill=COLOURS[-1],bg="white",bt="y",title="Did this prevent you from answering 100% accurately?")
        NUM <- format(min(as.matrix(H1.COUNTS[order(`No concern`)][,rowSums(.SD),by=.(Concern)],rownames="Concern")),big.mark=",")
        mtext(text=sprintf("Self-reported concerns about participating\n(n=%s)",NUM),font=2,cex=1.2,line=1)
        dev.off()        

        ##
        ## H2 <<Concern+Impact associated with sex/keystage/ethnicity>>

        
        TEMPc <- LONG[Question=="ConcernType",.(BINARY=c("No concerns","One to five concerns")[1+any(value=="Ticked (Yes)")]),by=.(SEX,KEYSTAGE,ETHNICONS,RID)]

        H2c.SEX.MOD <- glm( formula=I(BINARY=="One to five concerns") ~ 1 + SEX, family=binomial(link="logit"), data=TEMPc )
        H2c.ETHNIC.MOD <- glm( formula=I(BINARY=="One to five concerns") ~ 1 + ETHNICONS, family=binomial(link="logit"), data=TEMPc )
        H2c.KS.MOD <- glm( formula=I(BINARY=="One to five concerns") ~ 1 + KEYSTAGE, family=binomial(link="logit"), data=TEMPc )
        H2c.ALL.MOD <- glm( formula=I(BINARY=="One to five concerns") ~ 1 + SEX + KEYSTAGE + ETHNICONS, family=binomial(link="logit"), data=TEMPc )

        ## Difference with other, is whether to combine PNTS in with Yes (in terms of impact on accuracy)
        ##
        TEMPi <- LONG[TEMPc[BINARY=="One to five concerns",.(RID)],on=.(RID)][
            Question=="Impact",
            .(BINARY=c("No impact","One to five impacts")[1+any(value%in%c("Yes","Prefer not to say"))],
              BINARY.SUP=c("No impact","One to five impacts")[1+any(value%in%c("Yes"))]),
            by=.(SEX,KEYSTAGE,ETHNICONS,RID)]

        write.csv(TEMPi[,.N,by=.(BINARY,BINARY.SUP)],
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.supplementary-impact-counts-shift.csv",VERSION)) )
        
        ## For supplementary exclude PNTS (aka only a definitive yes counts for impact 0/1-5)
        TEMPiSUP <- LONG[TEMPc[BINARY=="One to five concerns",.(RID)],on=.(RID)][Question=="Impact",.(BINARY=c("No impact","One to five impacts")[1+any(value%in%c("Yes"))]),by=.(SEX,KEYSTAGE,ETHNICONS,RID)]

        H2i.SEX.MOD <- glm( formula=I(BINARY=="One to five impacts") ~ 1 + SEX, family=binomial(link="logit"), data=TEMPi )
        H2i.ETHNIC.MOD <- glm( formula=I(BINARY=="One to five impacts") ~ 1 + ETHNICONS, family=binomial(link="logit"), data=TEMPi )
        H2i.KS.MOD <- glm( formula=I(BINARY=="One to five impacts") ~ 1 + KEYSTAGE, family=binomial(link="logit"), data=TEMPi )
        H2i.ALL.MOD <- glm( formula=I(BINARY=="One to five impacts") ~ 1 + SEX + KEYSTAGE + ETHNICONS , family=binomial(link="logit"), data=TEMPi )
        
        H2iSUP.SEX.MOD <- glm( formula=I(BINARY.SUP=="One to five impacts") ~ 1 + SEX, family=binomial(link="logit"), data=TEMPi )
        H2iSUP.ETHNIC.MOD <- glm( formula=I(BINARY.SUP=="One to five impacts") ~ 1 + ETHNICONS, family=binomial(link="logit"), data=TEMPi )
        H2iSUP.KS.MOD <- glm( formula=I(BINARY.SUP=="One to five impacts") ~ 1 + KEYSTAGE, family=binomial(link="logit"), data=TEMPi )
        H2iSUP.ALL.MOD <- glm( formula=I(BINARY.SUP=="One to five impacts") ~ 1 + SEX + KEYSTAGE + ETHNICONS , family=binomial(link="logit"), data=TEMPi )

        


        tab_model(H2c.SEX.MOD, H2i.SEX.MOD, dv.labels=c("Concerns","Impacts"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-sex.html",VERSION)) )
        tab_model(H2c.ETHNIC.MOD, H2i.ETHNIC.MOD, dv.labels=c("Concerns","Impacts"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-ethnicity.html",VERSION)) )
        
        tab_model(H2c.KS.MOD, H2i.KS.MOD, dv.labels=c("Concerns","Impacts"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-keystage.html",VERSION)) )
        tab_model(H2c.ALL.MOD, H2i.ALL.MOD, dv.labels=c("Concerns","Impacts"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-all.html",VERSION)) )

        tab_model(H2c.SEX.MOD, H2i.SEX.MOD, H2iSUP.SEX.MOD, dv.labels=c("Concerns","Impacts (Y/PNTS)","Impacts (Y only)"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-sex-SUP.html",VERSION)) )
        tab_model(H2c.ETHNIC.MOD, H2i.ETHNIC.MOD, H2iSUP.ETHNIC.MOD, dv.labels=c("Concerns","Impacts (Y/PNTS)","Impacts (Y only)"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-ethnicity-SUP.html",VERSION)) )
        tab_model(H2c.KS.MOD, H2i.KS.MOD, H2iSUP.KS.MOD, dv.labels=c("Concerns","Impacts (Y/PNTS)","Impacts (Y only)"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-keystage-SUP.html",VERSION)) )
        tab_model(H2c.ALL.MOD, H2i.ALL.MOD, H2iSUP.ALL.MOD, dv.labels=c("Concerns","Impacts (Y/PNTS)","Impacts (Y only)"),
                  file=file.path(DIRS$PROJECT,sprintf("%s.H2.CI-all-SUP.html",VERSION)) )

    }


    if( 1 ) {
        ##
        ## H3 <<Presence of missing items on three key scales within OxWell associated with Concerns+Impacts>>
        ##
        
        SCMQ.SET <- c("X2440","X2430","X2420","X2410","X2400","X2390")
        
        for( BOB in SCMQ.SET ) {
            SUBSET[,c(sprintf("SCMQ.%s.missing.indicator",BOB)):=((rowSums(.SD=="NoResponse"))==4),.SDcols=paste0(BOB,c("a","b","c","d"))]

        }

        SUBSET[,SCMQ.any.missing:=apply(.SD,1,any),.SDcols=sprintf(c("SCMQ.%s.missing.indicator"),rep(SCMQ.SET,each=1))]
        SUBSET[,RCADS11.any.missing:=is.na(RCADS11.Overall.Complete)]
        SUBSET[,DAWBA.any.missing:=DAWBA.Screen.MISSING>0]
        
        
        MISSING.SET <- grep("any.missing",names(SUBSET),value=TRUE)

        RESULTS <- list()
        for( LABy in MISSING.SET ) {
            for( LABx in CONCERN.SET ) {
                MOD <- glm( as.formula( sprintf("%s ~ 1 + concern",LABy) ), family="binomial", data=SUBSET[,concern:=relevel(get(sprintf("X2770%s",LABx)),ref="NoResponse")] )
                concern.out <- sprintf("%s (%s)",format(exp(coef(MOD))["concernTicked (Yes)"],digits=3,nsmall=3),paste(format(exp(confint(MOD))["concernTicked (Yes)",],digits=3,nsmall=3),collapse=", "))
                concern.p <- summary(MOD)$coefficients[2,4]

                MOD <- glm( as.formula( sprintf("%s ~ 1 + accuracy",LABy) ), family="binomial", data=SUBSET[,accuracy:=relevel(get(sprintf("X2771%sV",LABx)),ref="No")][accuracy!="NoResponse" & accuracy!="No concern"] )

                accuracyYes.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyYes"],digits=3,nsmall=3),paste(format(exp(confint(MOD))["accuracyYes",],digits=3,nsmall=3),collapse=", "))
                accuracyPntS.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyPrefer not to say"],digits=3,nsmall=3),paste(format(exp(confint(MOD))["accuracyPrefer not to say",],digits=3,nsmall=3),collapse=", "))

                accuracyYes.p <- summary(MOD)$coefficients[2,4]
                accuracyPntS.p <- summary(MOD)$coefficients[3,4]
                
                ACC.TRANSLATE <- rbindlist( list(
                    data.table(old="No",new="No"),
                    data.table(old="Yes",new="Yes/PNTS"),
                    data.table(old="Prefer not to say",new="Yes/PNTS"),
                    data.table(old=c("NoResponse","No concern"),new=NA_character_)
                ))

                MOD <- glm( as.formula( sprintf("%s ~ 1 + accuracy",LABy) ), family="binomial", data=SUBSET[,accuracy5:=get(sprintf("X2771%sV",LABx))][ ACC.TRANSLATE, on=.(accuracy5=old), accuracy:=i.new ] )

                accuracyYesPNTS.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyYes/PNTS"],digits=3,nsmall=3),paste(format(exp(confint(MOD))["accuracyYes/PNTS",],digits=3,nsmall=3),collapse=", "))
                
                accuracyYesPNTS.p <- summary(MOD)$coefficients[2,4]
                
                RESULTS[[paste0(LABx,LABy)]] <- data.table(Concern=LABx,
                                                           Missing=LABy,
                                                           Type=c("concern","accuracyYesPNTS","SUPaccuaracyYes","SUPaccuracyPntS"),
                                                           out=c(concern.out,accuracyYesPNTS.out,accuracyYes.out,accuracyPntS.out),p=c(concern.p,accuracyYesPNTS.p,accuracyYes.p,accuracyPntS.p))
            }
        }
        H3.FINDINGS <- rbindlist(RESULTS)[ rbindlist(CONCERNS.LIST), on=.(Concern=CONCERN) ]
        H3.FINDINGS[Type=="concern",p.fdr:=p.adjust(p,"fdr")]
        H3.FINDINGS[Type=="accuracyYesPNTS",p.fdr:=p.adjust(p,"fdr")]
        H3.FINDINGS[Type%in%c("SUPaccuaracyYes","SUPaccuracyPntS"),p.fdr:=p.adjust(p,"fdr")]
        H3.FINDINGS[,Concern:=NULL]
        H3.FINDINGS[,P:=format(p,nsmall=5,scientific=FALSE,digits=0)]
        H3.FINDINGS[,P.fdr:=format(p.fdr,nsmall=5,scientific=FALSE,digits=0)]
        H3.FINDINGS[,.(i.Concern,Missing,Type,out,P,P.fdr)]
        write.csv(H3.FINDINGS[,.(i.Concern,Missing,Type,out,P,P.fdr)],
                  file=file.path(DIRS$PROJECT,sprintf("%s.H3.FINDINGS.csv",VERSION)))


        NSMALL <- 2
        RESULTSadj <- list()
        for( LABy in MISSING.SET ) {
            for( LABx in CONCERN.SET ) {
                MOD <- glm( as.formula( sprintf("%s ~ 1 + concern + SEX + KEYSTAGE + ETHNICONS",LABy) ), family="binomial", data=SUBSET[,concern:=relevel(get(sprintf("X2770%s",LABx)),ref="NoResponse")] )

                concern.est <- exp(coef(MOD))["concernTicked (Yes)"]
                concern.out <- sprintf("%s (%s)",format(exp(coef(MOD))["concernTicked (Yes)"],digits=NSMALL,nsmall=NSMALL),paste(format(exp(confint(MOD))["concernTicked (Yes)",],digits=NSMALL,nsmall=NSMALL),collapse=", "))
                concern.p <- summary(MOD)$coefficients[2,4]

                MOD <- glm( as.formula( sprintf("%s ~ 1 + accuracy + SEX + KEYSTAGE + ETHNICONS",LABy) ), family="binomial", data=SUBSET[,accuracy:=relevel(get(sprintf("X2771%sV",LABx)),ref="No")][accuracy!="NoResponse" & accuracy!="No concern"] )

                accuracyYes.est <- exp(coef(MOD))["accuracyYes"]
                accuracyYes.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyYes"],digits=NSMALL,nsmall=NSMALL),paste(format(exp(confint(MOD))["accuracyYes",],digits=NSMALL,nsmall=NSMALL),collapse=", "))

                accuracyPntS.est <- exp(coef(MOD))["accuracyPrefer not to say"]
                accuracyPntS.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyPrefer not to say"],digits=NSMALL,nsmall=NSMALL),paste(format(exp(confint(MOD))["accuracyPrefer not to say",],digits=NSMALL,nsmall=NSMALL),collapse=", "))

                accuracyYes.p <- summary(MOD)$coefficients[2,4]
                accuracyPntS.p <- summary(MOD)$coefficients[3,4]

                ACC.TRANSLATE <- rbindlist( list(
                    data.table(old="No",new="No"),
                    data.table(old="Yes",new="Yes/PNTS"),
                    data.table(old="Prefer not to say",new="Yes/PNTS"),
                    data.table(old=c("NoResponse","No concern"),new=NA_character_)
                ))

                MOD <- glm( as.formula( sprintf("%s ~ 1 + accuracy + SEX + KEYSTAGE + ETHNICONS",LABy) ), family="binomial", data=SUBSET[,accuracy5:=get(sprintf("X2771%sV",LABx))][ ACC.TRANSLATE, on=.(accuracy5=old), accuracy:=i.new ] )

                accuracyYesPNTS.est <- exp(coef(MOD))["accuracyYes/PNTS"]
                accuracyYesPNTS.out <- sprintf("%s (%s)",format(exp(coef(MOD))["accuracyYes/PNTS"],digits=NSMALL,nsmall=NSMALL),paste(format(exp(confint(MOD))["accuracyYes/PNTS",],digits=NSMALL,nsmall=NSMALL),collapse=", "))
                
                accuracyYesPNTS.p <- summary(MOD)$coefficients["accuracyYes/PNTS",4]

                

                RESULTSadj[[paste0(LABx,LABy)]] <- data.table(Concern=LABx,
                                                              Missing=LABy,
                                                              Type=c("concern","accuracyYesPNTS","SUPaccuaracyYes","SUPaccuracyPntS"),
                                                              est=c(concern.est,accuracyYesPNTS.est,accuracyYes.est,accuracyPntS.est),
                                                              out=c(concern.out,accuracyYesPNTS.out,accuracyYes.out,accuracyPntS.out),
                                                              p=c(concern.p,accuracyYesPNTS.p,accuracyYes.p,accuracyPntS.p))
            }
        }
        H3adj.FINDINGS <- rbindlist(RESULTSadj)[ rbindlist(CONCERNS.LIST), on=.(Concern=CONCERN) ]
        H3adj.FINDINGS[Type=="concern",p.fdr:=p.adjust(p,"fdr")]
        H3adj.FINDINGS[Type=="accuracyYesPNTS",p.fdr:=p.adjust(p,"fdr")]
        H3adj.FINDINGS[Type%in%c("SUPaccuaracyYes","SUPaccuracyPntS"),p.fdr:=p.adjust(p,"fdr")]
        H3adj.FINDINGS[,Concern:=NULL]
        H3adj.FINDINGS[,P:=format(p,nsmall=5,scientific=FALSE,digits=0)]
        H3adj.FINDINGS[,P.fdr:=format(p.fdr,nsmall=5,scientific=FALSE,digits=0)]
        H3adj.FINDINGS[,.(i.Concern,Missing,Type,out,P,P.fdr)]
        write.csv(H3adj.FINDINGS[,.(i.Concern,Missing,Type,out,P,P.fdr)],
                  file=file.path(DIRS$PROJECT,sprintf("%s.H3adj.FINDINGS.csv",VERSION)))
        saveRDS(H3adj.FINDINGS[,.(i.Concern,Missing,Type,out,P,P.fdr)],
                file=file.path(DIRS$PROJECT,sprintf("%s.H3adj.FINDINGS.rds",VERSION)))        
        
    }
    

    if( 1 ) {
        ##
        ## H4 <<Effect on total score on three key scales within OxWell associated with Concerns+Impacts>>
        ##

        SCMQ.SET <- c("X244","X243","X242","X241","X240","X239")
        
        for( BOB in SCMQ.SET ) {
            SUBSET[ SCMQ.any.missing==FALSE, c(sprintf("SCMQ.%s.score",BOB)):=0 ]

            SUBSET[ (get(sprintf("%s0b",BOB))=="Ticked (Yes)"), c(sprintf("SCMQ.%s.score",BOB)):=1 ]
            SUBSET[ (get(sprintf("%s0c",BOB))=="Ticked (Yes)"), c(sprintf("SCMQ.%s.score",BOB)):=1 ]
            SUBSET[ (get(sprintf("%s0b",BOB))=="Ticked (Yes)") & (get(sprintf("%s1",BOB))=="Many times"), c(sprintf("SCMQ.%s.score",BOB)):=2 ]
            SUBSET[ (get(sprintf("%s0c",BOB))=="Ticked (Yes)") & (get(sprintf("%s2",BOB))=="Many times"), c(sprintf("SCMQ.%s.score",BOB)):=2 ]
            
        }
        
        SUBSET[,SCMQ.score:=apply(.SD,1,sum),.SDcols=sprintf("SCMQ.%s.score",SCMQ.SET)]
        SUBSET[,table(SCMQ.score,exclude=NULL)]

        SUBSET[,RCADS11.score:=RCADS11.Overall.Complete] ## RCADS (inc impact)
        SUBSET[,DAWBA.score:=DAWBA.Screen.RAW]


        SUBSET[DAWBA.Screen.MISSING>0,DAWBA.score:=NA_integer_]
        

        SCORE.SET <- c("SCMQ.score", "RCADS11.score", "DAWBA.score") ## "Substance.score", ,"PLEs.score"

        SCORE.RESULTS <- list()
        for( LABy in SCORE.SET ) {
            for( LABx in CONCERN.SET ) {
                SCORE.RESULTS[[paste0(LABx,LABy)]] <- SUBSET[,c(Score=LABy,CONCERN=LABx,N=.N,Missing=sum(is.na(as.numeric(get(LABy)))),as.list(quantile(as.numeric(get(LABy)),prob=c(0.25,0.5,0.75,1),na.rm=TRUE))),by=c(Level=sprintf("X2771%sV",LABx))]
            }
        }

        
        H4.FINDINGS <- rbindlist(SCORE.RESULTS)[ rbindlist(CONCERNS.LIST), on=.(CONCERN) ]

        setcolorder(H4.FINDINGS,neworder="Concern")
        write.csv(H4.FINDINGS,file=file.path(DIRS$PROJECT,sprintf("%s.H4.FINDINGS.csv",VERSION)))


        

        ##
        ## Major figure
        ##
        COLOURS <- c("Yes"="#1B9E77", ## 
                     "Prefer not to say"="#7570B3", ## 
                     "No"="#E6AB02", ## Neon Fuchsia 
                     "No concern"="#000000")  

        pdf(file=file.path(DIRS$PROJECT,sprintf("%s.H4.step.pdf",VERSION)),width=40/cm(1), height=30/cm(1))
        layout( matrix( 1:(length(SCORE.SET)*length(CONCERN.SET)), nrow=length(CONCERN.SET) ) )
        par(oma=c(4,6,3,0))
        for( LABy in SCORE.SET ) {
            for( LABx in CONCERN.SET ) {
                TMP1 <- SUBSET[,.(Freq=.N),by=c(Score=LABy,Level=sprintf("X2771%sV",LABx))]
                TABLE <- dcast.data.table(data=TMP1[,.(Level,Freq,ScoreNA=factor(is.na(Score),c(FALSE,TRUE),c("Scored","ScoreNA")))],formula=Level ~ ScoreNA, fill=0, value.var="Freq", fun.aggregate=sum, drop=c(TRUE,FALSE))[,Prop:=round(100*ScoreNA/(ScoreNA+Scored),1)]
                TMP <- TMP1[!is.na(Score)][,.(Freq,Prop=Freq/sum(Freq),Score=Score),by=.(Level)][order(Level,Score)]

                ADTEST <- ad.test( V1 ~ Level, data=droplevels(SUBSET[,(get(LABy)),by=.(Level=get(sprintf("X2771%sV",LABx)))][Level!="NoResponse"]), Nsim=2000 )
                
                TMP[,FROM:=Score-0.5]
                TMP[,TO:=Score+0.5]
                XMAX <- c("SCMQ.score"=12,"RCADS11.score"=39,"DAWBA.score"=5)[LABy]
                YMAX <- c("SCMQ.score"=0.8,"RCADS11.score"=0.1,"DAWBA.score"=0.5)[LABy]
                TITLE <- c("SCMQ.score"="SCMQ [0-12]","RCADS11.score"="RCADS [0-39]","DAWBA.score"="DAWBA [0-5]")[LABy]
                par(mar=c(2,2,0.5,0.5))
                plot(0,type="n",xlim=c(0-0.5,XMAX+0.5),ylim=c(0,YMAX),xlab="",ylab="",xaxt="n",xaxs="i",yaxs="i")
                if(LABx=="a")mtext(text=TITLE,side=3,line=1,outer=FALSE)
                if(LABy=="SCMQ.score")mtext(text=sapply(strwrap(rbindlist(CONCERNS.LIST)[CONCERN==LABx,Concern],width=23,simplify=FALSE),FUN=paste0,collapse="\n"),side=2,line=3,adj=0.5,cex=0.8)
                
                COUNTS <- TMP[,sum(Freq),by=.(Level)][na.omit(match(names(COLOURS),Level)),.(Level=Level,Count=format(V1,big.mark=","))]
                max(strwidth(COUNTS$Count))
                text(x=grconvertX(1,"npc","user")-0.25*strwidth("x"),y=grconvertY(1,"npc","user")-(1.2*strheight("M")*seq_len(NROW(COUNTS))),labels=COUNTS$Count,adj=c(1,1))
                text(x=grconvertX(1,"npc","user")-max(strwidth(COUNTS$Count))-strwidth("M"),y=grconvertY(1,"npc","user")-(1.2*strheight("M")*seq_len(NROW(COUNTS))),labels=COUNTS$Level,adj=c(1,1))

                text(x=grconvertX(1,"npc","user")-0.25*strwidth("x"),y=grconvertY(1,"npc","user")-(1.2*strheight("M")*(2+NROW(COUNTS))),
                     labels=sprintf("Anderson-Darling Test: %s",ifelse(ADTEST$ad[1,3]<1e-4,"<0.0001",format(ADTEST$ad[1,3],scientific=FALSE,nsmall=4))),
                     adj=c(1,1))

                axis(side=1,labels=TRUE)
                TMP[,segments(x0=Score-0.5,x1=Score+0.5,y0=Prop, lwd=4, col=COLOURS[as.character(.BY[[1]])]),by=.(Level)]
            }
        }
        legend(x=grconvertX(0.5,"ndc","user"),y=grconvertY(0,"ndc","user"),horiz=TRUE,legend=names(COLOURS),col=COLOURS,lwd=5,lty=1,xpd=NA,xjust=0.5,yjust=0,
               title="Did this prevent you from answering 100% accurately?")
        dev.off()
    }



}


