# Read your gene expression matrix (GEdata) with samples in columns and genes in rows.
# Entrez ID must be used as gene IDs

sig_score<-function(signature.file, GEdata){
  #signature.file = iGES.txt
  sigdat <- read.table(signature.file,sep='\t',header=F,row.names=1,fill=T)
  outdat<-NULL
  names.row<-NULL
  for(i in 1:nrow(sigdat)){
    rows<- which(rownames(GEdata) %in% sigdat[i,])
    if(length(rows) > 1){
      sigscore<-apply(GEdata[rows,],2,median,na.rm=TRUE)
      outdat<-rbind(outdat,sigscore)
      names.row<-rbind(names.row,rownames(sigdat)[i])
    }
    if(length(rows) == 1){
      sigscore<-as.numeric(GEdata[rows,])
      outdat<-rbind(outdat,sigscore)
      names.row<-rbind(names.row,rownames(sigdat)[i])
    }
  }
  rownames(outdat)<-names.row
  colnames(outdat)<-colnames(GEdata)
  return(outdat)
}

# signatures<-sig_score("iGES.txt",GEdata)
