# Medalla attestation visualization

To get the attestation behavior for each validator at each epoch, I used a bash script that queried my local Lighthouse beacon node. The script first gets the public addresses of all validators for an epoch and then queries for the attestation status for the validators for that epoch. The below script was run with $1 giving the starting epoch, $2 the ending epoch and $3 name for temporary storage of files during the run.

```bash
for i in $(eval echo {$1..$2})
do
	echo "{" > $HOME/$3.json
	echo ""\"epoch"\": $i," >> $HOME/$3.json
	echo ""\"pubkeys"\": [" >> $HOME/$3.json
	curl "localhost:5052/validator/duties/all?epoch=$i" | tr '{' '\n' > $HOME/$3tmp
	num_of_lines=$(($(< $HOME/$3tmp wc -l)+1))
	awk -v num=$num_of_lines -F",|:" '{ if(NR==num) print $2; else if(NR>1) print $2"," }' $HOME/$3tmp >> $HOME/$3.json
	echo "]" >> $HOME/$3.json
	echo "}" >> $HOME/$3.json
	curl "localhost:5052/consensus/individual_votes" -d @$3.json |  tr '{' '\n' | awk -F",|:" 'NR>1 && NR%2 && $12=="true" {print "1"}; NR>1 && NR%2 && $12=="false" {print "0"}' | paste -s -d, - > $HOME/epochs/$i.txt
done
```

Note that the above api query will only work with Lighthouse versions before 0.3 when the api endpoints were changed.

The /epochs/ folder containing one .txt for each epoch was then downloaded to "C:/R/epoch/" and processed in R into a matrix with epochs as rows and validators as columns, stored as "epochDat", total 1.7 gb.

```R
library(data.table)

files = list.files("C:/R/epoch/")
files = unlist(strsplit(files,".",fixed=T))[c(T,F)]
files = files[!(is.na(as.numeric(files)))]
datIn = list()
ncolVec = c()
for (file in files) {
	datTmp = fread(paste0("C:/R/epoch/",file,".txt"),header=F,sep=",")
	colnames(datTmp) = as.character(0:(ncol(datTmp)-1))
	datIn[[file]] = datTmp
}
datIn = rbindlist(datIn,fill=T,idcol="epoch")
save(datIn,file="C:/R/epochDat") # Location epochDat
```

The epochs were then processed in batches of 1000 epochs each, with an increment of 20 epochs per run. Principle components were calculated. The sign of each principle components when it is calculated has a random component so I made a script to harmonize the sign of the PC1 and PC and thereby allow animation.

```R
library(data.table)
library(ggplot2)
library(anytime)
timeEpoch = function(x) anytime("08/04/2020 15:00:08")+x*384

load("C:/R/epochDat") # Location epochDat
sampName = "201014 ep1000 script" # Name run
dir.create(paste0("C:/R/",sampName,"/")) # Set output folder

# 500
#start = seq(1,13501,by=20)
#stop = seq(499,13999,by=20)
#setSel = c(1,676)
# 1000
start = seq(1,13001,by=20)
stop = seq(999,13999,by=20)
setSel = c(1,651)
# 1500
#start = seq(1,12501,by=20)
#stop = seq(1499,13999,by=20)
#setSel = c(1,626)

plotList = list()
for (set in setSel[1]:setSel[2]) {
	print(paste(Sys.time(),"starting",set))
	setName = paste(start[set],stop[set],sep="_")
	tmp = as.matrix(datIn[start[set]:stop[set]],rownames=T)
	tmp = tmp[,colSums(is.na(tmp)*1)==0]
	pca_res = prcomp(t(tmp))
	pcaDf = data.frame(val=rownames(pca_res$x),set=set,setName=setName,PC1=pca_res$x[,1],PC2=pca_res$x[,2],
		"last20mean"=colMeans(tmp[(nrow(tmp)-20):nrow(tmp),]),"last1"=tmp[nrow(tmp),],"timeVec"=timeEpoch(stop[set]))
	
	# Make sure that the first set of epochs is oriented so that sucessful validators are in the left corner and unsuccesful are in the right
	if (set == 1) {
		if (mean(pcaDf[order(pcaDf[,"PC1"])[1:100],"last20mean"]) < mean(pcaDf[order(pcaDf[,"PC1"],decreasing=T)[1:100],"last20mean"])) {
			pcaDf[,"PC1"] = pcaDf[,"PC1"] * (-1)
		}
		if (mean(pcaDf[order(pcaDf[,"PC2"])[1:100],"last20mean"]) < mean(pcaDf[order(pcaDf[,"PC2"],decreasing=T)[1:100],"last20mean"])) {
			pcaDf[,"PC2"] = pcaDf[,"PC2"] * (-1)
		}
	}
	# The following sets of epochs preserve the same "sign" as the first one, to allow animation
	if (set > 1) {
		prevPosDif = abs(pcaDf[1:length(prevPc1),"PC1"] - prevPc1)
		prevNegDif = abs(pcaDf[1:length(prevPc1),"PC1"]*(-1) - prevPc1)
		if (sum(prevPosDif) > sum(prevNegDif)) pcaDf[,"PC1"] = pcaDf[,"PC1"] * (-1)
		prevPosDif = abs(pcaDf[1:length(prevPc1),"PC2"] - prevPc2)
		prevNegDif = abs(pcaDf[1:length(prevPc1),"PC2"]*(-1) - prevPc2)
		if (sum(prevPosDif) > sum(prevNegDif)) pcaDf[,"PC2"] = pcaDf[,"PC2"] * (-1)
	}
  
	p = ggplot(pcaDf,aes(x=PC1,y=PC2,group=val,color=last20mean)) +
		geom_point(alpha=0.075) +
		theme_void() +
		scale_colour_gradient(low = "red", high = "black") +
		theme(legend.position="none") +
		ggtitle(strsplit(as.character(pcaDf[1,"timeVec"])," ")[[1]][1])
	ggsave(filename=paste0("C:/R/",sampName,"/",set,".png"),plot=p,width=15,height=15,units="cm",dpi=200)
	
	prevPc1 = pcaDf[,"PC1"]
	prevPc2 = pcaDf[,"PC2"]
}
```

Animation of the produced .png was done by the windows command line tool [gifski](https://github.com/ImageOptim/gifski). The best size of epochs sets for the animation was compared:

500 epochs per frame with 20 epochs increment per frame: | 1000 epochs per frame with 20 epochs increment per frame: | 1500 epochs per frame with 20 epochs increment per frame:
:-------------------------:|:-------------------------:|:-------------------------:
![500](https://github.com/petclippy/medalla/blob/main/500_90_400.gif?raw=true)  |  ![1000](https://github.com/petclippy/medalla/blob/main/1000scr_90_400.gif?raw=true) | ![1500](https://github.com/petclippy/medalla/blob/main/1500_90_400.gif?raw=true) 

The longer the range of epochs used, the less responsive the analysis will be to current events if that is the goal of this visualization. However, it is also clear that shorter epoch ranges makes the visualization less stable and more difficult to visually get information from. For further analysis I landed on using 1000 epoch ranges as it seems to give a good compromise of relative stability while it should also be possible to get a visualization of the current state of the network after some hours.

To identify clusters of validators, hierarchical clustering of the principal components was used, cut a different heights to explore clusters at several levels of organization.

```R
library(Rclusterpp)
library(data.table)
library(anytime)
library(ggplot2)
timeEpoch = function(x) anytime("08/04/2020 15:00:08")+x*384

load("C:/R/epochDat") # Location epochDat
sampName = "201014 ep1000 script" # Name run
dir.create(paste0("C:/R/",sampName,"/")) # Set output folder

# 1000
start = seq(1,13001,by=20)
stop = seq(999,13999,by=20)
setSel = c(1,651)

for (set in setSel[1]:setSel[2]) {
	print(paste(Sys.time(),"starting",set))
	setName = paste(start[set],stop[set],sep="_")
	tmp = as.matrix(datIn[start[set]:stop[set]],rownames=T)
	tmp = tmp[,colSums(is.na(tmp)*1)==0]
	pca_res = prcomp(t(tmp))
	
	distIn = dist(data.frame(PC1=pca_res$x[,1],PC2=pca_res$x[,2]))
	hclustTmp = Rclusterpp.hclust(distIn,method="complete")
	hCut2 = cutree(hclustTmp, h=2)
	hCut3 = cutree(hclustTmp, h=3)
	hCut4 = cutree(hclustTmp, h=4)
	hCut5 = cutree(hclustTmp, h=5)
	hCut6 = cutree(hclustTmp, h=6)
	hCut8 = cutree(hclustTmp, h=8)
	hCut10 = cutree(hclustTmp, h=10)
	
	pcaDf = data.frame(val=rownames(pca_res$x),set=set,setName=setName,PC1=pca_res$x[,1],PC2=pca_res$x[,2],
		"last20mean"=colMeans(tmp[(nrow(tmp)-20):nrow(tmp),]),"last1"=tmp[nrow(tmp),],"timeVec"=timeEpoch(stop[set]),
		"cl2"=hCut2,"cl3"=hCut3,"cl4"=hCut4,"cl5"=hCut5,"cl6"=hCut6,"cl8"=hCut8,"cl10"=hCut10)
	
	save(pcaDf,file=paste0("C:/R/",sampName,"/",set))
}
```

The clusters were first explored and visualized on the plots to select the most interesting clusters. 

```R
library(ggpubr)

sampName = "201014 ep1000 script" # Name run

epSel = 200
clustType = "cl2"

load(paste0("C:/R/",sampName,"/",epSel))

# Visualize the clusters
ggscatter(data=pcaDf,x="PC1",y="PC2",color=clustType)

# Select clusters with more than 50 validators
clSizes = sapply(as.character(unique(pcaDf[,clustType])),function(x) nrow(pcaDf[pcaDf[,clustType]==x,]))
clust100 = clSizes[clSizes>50]

# Get the pc1 and pc2 means of the clusters. Use these values to select specific clusters to follow
pc1tmp = sapply(names(clust100),function(x) mean(pcaDf[pcaDf[,clustType]==x,"PC1"]))
pc2tmp = sapply(names(clust100),function(x) mean(pcaDf[pcaDf[,clustType]==x,"PC2"]))

# Set the runList vectors. First field is the level of clustering, second is the epoch that contains the cluster you want.
# Third field is the starting epoch for the animation. Forth field is the end of the animation and sixth field is the cluster identifier
runList = list()
runList[[1]] = c("cl4",111,50,180,18)
runList[[2]] = c("cl2",200,181,246,4)
runList[[3]] = c("cl2",30,1,60,6)
runList[[4]] = c("cl2",30,1,60,8)
runList[[5]] = c("cl2",30,28,90,29)
runList[[6]] = c("cl2",200,200,246,32)
runList[[7]] = c("cl2",200,208,246,46)
runList[[8]] = c("cl2",30,12,60,49)
runList[[9]] = c("cl2",200,150,246,59)
runList[[10]] = c("cl2",200,175,230,65)
runList[[11]] = c("cl2",30,1,100,66)
runList[[12]] = c("cl2",30,1,100,70)
runList[[13]] = c("cl2",200,175,235,83)
runList[[14]] = c("cl2",200,150,246,93)
runList[[15]] = c("cl2",200,181,246,151)

for (run in 1:length(runList)) {
	clustType = runList[[run]][1]
	epRange = seq(runList[[run]][3],runList[[run]][4])
	valSet = runList[[run]][5]
	epSel = runList[[run]][2]
	load(paste0("C:/R/",sampName,"/",epSel))
	vals = pcaDf[pcaDf[,clustType]==valSet,"val"]
	for (set in epRange) {
		load(paste0("C:/R/",sampName,"/",set))
		
		if (set == epRange[1]) {
			if (mean(pcaDf[order(pcaDf[,"PC1"])[1:100],"last20mean"]) < mean(pcaDf[order(pcaDf[,"PC1"],decreasing=T)[1:100],"last20mean"])) {
				pcaDf[,"PC1"] = pcaDf[,"PC1"] * (-1)
			}
			if (mean(pcaDf[order(pcaDf[,"PC2"])[1:100],"last20mean"]) < mean(pcaDf[order(pcaDf[,"PC2"],decreasing=T)[1:100],"last20mean"])) {
				pcaDf[,"PC2"] = pcaDf[,"PC2"] * (-1)
			}
		}
		if (set > epRange[1]) {
			prevPosDif = abs(pcaDf[1:length(prevPc1),"PC1"] - prevPc1)
			prevNegDif = abs(pcaDf[1:length(prevPc1),"PC1"]*(-1) - prevPc1)
			if (sum(prevPosDif) > sum(prevNegDif)) pcaDf[,"PC1"] = pcaDf[,"PC1"] * (-1)
			prevPosDif = abs(pcaDf[1:length(prevPc2),"PC2"] - prevPc2)
			prevNegDif = abs(pcaDf[1:length(prevPc2),"PC2"]*(-1) - prevPc2)
			if (sum(prevPosDif) > sum(prevNegDif)) pcaDf[,"PC2"] = pcaDf[,"PC2"] * (-1)
		}
		p = ggplot(pcaDf,aes(x=PC1,y=PC2,color=last20mean)) + 
			geom_point(data=pcaDf[pcaDf[,"val"] %in% vals,],aes(x=PC1,y=PC2,color=last20mean),alpha=0.5,size=2) +
			geom_point(data=pcaDf[!(pcaDf[,"val"] %in% vals),],aes(x=PC1,y=PC2,color=last20mean),alpha=0.01,size=0.5) +
			theme_void() +
			scale_colour_gradient(low = "red", high = "black") +
			theme(legend.position="none") +
			ggtitle(paste(strsplit(as.character(pcaDf[1,"timeVec"])," ")[[1]][1],
					"\nEpochs:",paste(unlist(strsplit(as.character(pcaDf[1,"setName"]),"_")),collapse=" - "),
					"\nCluster",valSet,"with",length(vals),"validators"))
		ggsave(filename=paste0("C:/R/",sampName,"/",valSet,"_",set,".png"),plot=p,width=15,height=15,units="cm",dpi=200)
		
		prevPc1 = pcaDf[,"PC1"]
		prevPc2 = pcaDf[,"PC2"]
	}
}
```

Cluster 18 from epoch set 111  | Cluster 4 from epoch set 200 | Cluster 6 from epoch set 30
:-------------------------:|:-------------------------:|:-------------------------:
![cl4_111_clust18_75_300](https://github.com/petclippy/medalla/blob/main/cl4_111_clust18_75_300_rep.gif?raw=true)  |  ![cl2_200_clust4_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust4_75_300.gif?raw=true) | ![cl2_30_clust6_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust6_75_300.gif?raw=true) 

Cluster 8 from epoch set 30 | Cluster 29 from epoch set 30 | Cluster 32 from epoch set 200
:-------------------------:|:-------------------------:|:-------------------------:
![cl2_30_clust8_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust8_75_300.gif?raw=true)  |  ![cl2_30_clust29_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust29_75_300.gif?raw=true) | ![cl2_200_clust32_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust32_75_300.gif?raw=true) 


Cluster 46 from epoch set 200 | Cluster 49 from epoch set 30 | Cluster 59 from epoch set 200
:-------------------------:|:-------------------------:|:-------------------------:
![cl2_200_clust46_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust46_75_300.gif?raw=true)  |  ![cl2_30_clust49_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust49_75_300.gif?raw=true) | ![cl2_200_clust59_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust59_75_300.gif?raw=true) 


Cluster 65 from epoch set 200 | Cluster 65 from epoch set 30 | Cluster 70 from epoch set 30
:-------------------------:|:-------------------------:|:-------------------------:
![cl2_200_clust65_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust65_75_300.gif?raw=true)  |  ![cl2_30_clust65_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust65_75_300.gif?raw=true) | ![cl2_30_clust70_75_300](https://github.com/petclippy/medalla/blob/main/cl2_30_clust70_75_300.gif?raw=true) 


Cluster 93 from epoch set 200 | Cluster 151 from eopch set 200 | Cluster 4 from epoch set 200
:-------------------------:|:-------------------------:|:-------------------------:
![cl2_200_clust93_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust93_75_300.gif?raw=true)  |  ![cl2_200_clust151_75_300](https://github.com/petclippy/medalla/blob/main/cl2_200_clust151_75_300.gif?raw=true) | ![cl2_200_clust4_90_400_rep](https://github.com/petclippy/medalla/blob/main/cl2_200_clust4_90_400_rep.gif?raw=true)


If all validators that are linked together by being controlled by the same user could be correctly defined, the clusters could be used to derive economic analysis of the network like calculating the concentration of wealth and power in the network. The princple component cluster attribution (shown above) will not be able to correctly identify all clusters and there are confounding factors that may lead to misattribution of validators in clusters. However, there is also no other established way to gather this type of economic network information so it is interesting to explore using this type of analysis. 

The further cluster analysis uses hierarchical clustering cut height set to 4. Setting this value to 4 is somewhat arbitrary and was decided by visually inspecting several epochs and finding the height cut value that corresponds best to what I feel are the strongest visually defined clusters in the principle components.

The by far two biggest clusters in any set of epochs was found to be the clusters at the corners with successful attestation and unsuccessful attestation for the given range of epochs. These clusters are not iformative to group validators together so they are excluded in the further clustering of validators. All the remaining clusters were recorded for each set of epochs.

```R
library(ggplot2)
library(data.table)

# First generate clustDf that contains all epochs as rows and attribution to cluster for each validator in columns
clustList = list()
clustType = "cl4"
for (run in 1:651) {
	load(paste0("C:/R/",sampName,"/",run))
	biggestClusts = order(table(pcaDf[,clustType]),decreasing=T)[c(1,2)]
	clustList[[run]] = data.frame(t(pcaDf[,clustType]))
	clustList[[run]][1,clustList[[run]][1,] %in% biggestClusts] = NA
	colnames(clustList[[run]]) = pcaDf[,"val"]
}
clustDf = rbindlist(clustList,fill=T)

# Create clustOut list that contains links between validators that are always attributed to the same clusters over all non-NA epochs
clustOut = list()
for (val in colnames(clustDf)) {
	searStart = clustDf[[val]]
	searNAs = is.na(searStart)
	out = c()
	for (valLook in (as.numeric(val)+1):ncol(clustDf)) {
		if (val==valLook) next
		lookStart = clustDf[[valLook]]
		lookNAs = is.na(lookStart)
		if (length(searStart[searNAs+lookNAs==0])<2) next
		sear = searStart[searNAs+lookNAs==0]
		look = lookStart[searNAs+lookNAs==0]
		if (paste(sear,collapse=",")==paste(look,collapse=",")) out = c(out,as.character(valLook))
	}
	clustOut[[val]] = out
}

# Finally define the clusters and attribute each validator to only one cluster, the biggest cluster they are found to belong to
clustSize = sapply(clustOut,length)
clustOrder = order(clustSize,decreasing=T)

clustSelected = setNames(rep(0,length(colnames(clustDf))),as.character(colnames(clustDf)))
clusters = list()
clustCounter = 1
for (clust in 1:length(clustOrder)) {
	notPrevClust = clustSelected[clustOut[[clustOrder[clust]]]][clustSelected[clustOut[[clustOrder[clust]]]]==0]
	clusters[[clustCounter]] = names(notPrevClust)
	clustSelected[names(notPrevClust)] = 1
	clustCounter = clustCounter+1
}


clustSize = sapply(clusters,length)
clustOrder = order(clustSize)

plotDf = data.frame(size=(1:length(clustSize))/length(clustSize),sums=cumsum(clustSize[clustOrder])/sum(clustSize[clustOrder]),lab=clustSize[clustOrder])
p = ggplot(plotDf,aes(x=size,y=sums)) +
	geom_point() +
	geom_text_repel(data=plotDf[plotDf[,"lab"]>1000,],aes(label=lab)) +
	theme_minimal() +
	xlab("Cumulative participants") +
	ylab("Cumulative validator control (wealth)") +
	labs(title="Lorenz curve showing distribution of validators in clusters",subtitle="Cluster sizes with >1000 validators indicated")
	
ggsave(filename=paste0("C:/R/",sampName,"/lorenzCurve.png"),plot=p,width=15,height=15,units="cm",dpi=200)
```
![Lorenz Curve](https://github.com/petclippy/medalla/blob/main/lorenzCurve.png?raw=true) 

This plot is called a Lorenz curve and is a common way to illustrate inequality of distribution or wealth within a system. A straight line from bottom left to top right would be fully equal. The general conclusion from this analysis is that the network is very unequally distributed, as is expected for this network.

Note that this inequality analysis is preliminary. This framework will never be able to correctly attribute all clusters of validators correctly to individual holders of the crypto. However, because of the general difficulty of getting this type of information, I hope it may still be interesting and if nothing else spark discussion on how to improve it or other ideas on how to do this type of analysis.
