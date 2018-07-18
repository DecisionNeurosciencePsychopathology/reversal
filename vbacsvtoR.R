lfilepath<-system("find /Volumes/bek/reversal/vba_output -name '*.csv' -maxdepth 2 -mindepth 1 -type f",intern = T)
sppath<-strsplit(lfilepath,split = "/")
sapply(sppath, "[[",6)->filenames
substr(filenames,start = 3+regexpr("id_",filenames),stop = regexpr("_rev",filenames)-1)->IDLIST

tempenvir<-as.environment(list())
for (i in 1:length(lfilepath)) {
  tep<-read.csv(lfilepath[i])
  names(tep)<-c("value","PE")
  tep$ID<-IDLIST[i]
  tep$absValue<-abs(tep$value)
  assign(IDLIST[i],tep,envir = tempenvir)
}

for (z in 1:length(as.list(tempenvir))) {
  if (z==1) {as.list(tempenvir)[[z]]->allx}
  rbind(allx,as.list(tempenvir)[[z]])->allx
}
