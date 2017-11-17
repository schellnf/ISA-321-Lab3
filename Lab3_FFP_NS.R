install.packages("lpSolve")
install.packages("lpSolveAPI")
library(lpSolve)
library(lpSolveAPI)
ffp<-read.csv("Lab3_NS.csv")
ffp <- ffp[-201,]
as.data.frame(ffp)

objfn <- ffp$Projected.Points
objfn[198:200] <- 0
table(objfn)

LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6))
RHS <- c(200,1,2,3,1,1,1)
sol <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)

sol$objval
sol$solution 
ffp$solution1 <-sol$solution

Solution1ForFadel <-subset(ffp,solution1==1,select = c("Player.Name","Position","Projected.Points"))


#For solutions 2-10 : Repeat the above process, then add constraints by adding the most recent solution to the LHS matrix, another sign, and then a value for RHS

#Solution 2
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1),nrow=200, byrow=FALSE))
LHS[8,199]<-0
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

table(LHS[8,])
colnames(LHS) <- ffp$Player.Name

sign <- c("<=",rep("=",times=6), rep("<=",times=1))
RHS <- c(200,1,2,3,1,1,1,8)
sol_2 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_2$objval
sol_2$solution 
ffp$solution2 <-sol_2$solution
table(ffp$solution2)

Solution2ForFadel <-subset(ffp,solution2==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 3
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name

LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=2))
RHS <- c(200,1,2,3,1,1,1,8,7)
sol_3 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_3$objval
sol_3$solution 
ffp$solution3 <-sol_3$solution

Solution3ForFadel <-subset(ffp,solution3==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 4
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,ffp$solution3),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=3))
RHS <- c(200,1,2,3,1,1,1,8,7,7)
sol_4 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_4$objval
sol_4$solution 
ffp$solution4 <-sol_4$solution

Solution4ForFadel <-subset(ffp,solution4==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 5
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=4))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7)
sol_5 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_5$objval
sol_5$solution 
ffp$solution5 <-sol_5$solution

Solution5ForFadel <-subset(ffp,solution5==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 6
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4,ffp$solution5),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=5))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7,8)
sol_6 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_6$objval
sol_6$solution 
ffp$solution6 <-sol_6$solution

Solution6ForFadel <-subset(ffp,solution6==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 7 
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4,ffp$solution5,ffp$solution6),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=6))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7,8,7)
sol_7 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_7$objval
sol_7$solution 
ffp$solution7 <-sol_7$solution

Solution7ForFadel <-subset(ffp,solution7==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 8
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4,ffp$solution5,ffp$solution6,ffp$solution7),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=7))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7,8,7,7)
sol_8 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_8$objval
sol_8$solution 
ffp$solution8 <-sol_8$solution

Solution8ForFadel <-subset(ffp,solution8==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 9
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4,ffp$solution5,ffp$solution6,ffp$solution7,ffp$solution8),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=8))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7,8,7,7,8)
sol_9 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_9$objval
sol_9$solution 
ffp$solution9 <-sol_9$solution

Solution9ForFadel <-subset(ffp,solution9==1,select = c("Player.Name","Position","Projected.Points"))

#Solution 10 
LHS <- t(matrix(c(ffp$Cost, ffp$QB, ffp$RB, ffp$WR, ffp$TE, ffp$Def, ffp$Flex, ffp$solution1, ffp$solution2,
                  ffp$solution3,ffp$solution4,ffp$solution5,ffp$solution6,ffp$solution7,ffp$solution8,ffp$solution9),nrow=200, byrow=FALSE))
colnames(LHS) <- ffp$Player.Name
LHS[3,198] = -1
LHS[4,199] = -1
LHS[5,200] = -1

sign <- c("<=",rep("=",times=6), rep("<=",times=9))
RHS <- c(200,1,2,3,1,1,1,8,7,7,7,8,7,7,8,7)
sol_10 <- lp("max",objfn,LHS,sign,RHS,all.bin = TRUE)
sol_10$objval
sol_10$solution 
ffp$solution10 <-sol_10$solution

Solution10ForFadel <-subset(ffp,solution10==1,select = c("Player.Name","Position","Projected.Points"))

sol$objval      #175.4
sol_2$objval    #175.2
sol_3$objval    #175.2
sol_4$objval    #174.9
sol_5$objval    #174.9
sol_6$objval    #174.8
sol_7$objval    #174.5
sol_8$objval    #174.5
sol_9$objval    #174.4
sol_10$objval   #174.4
