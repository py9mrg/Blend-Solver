require(limSolve)
require(xlsx)
require(gdata)
require(shinyIncubator)
require(shinyTable)

calculate <- function(raw_data,totals,constraints,weights_x,weights_a) {
  par_mat <- as.matrix(raw_data[,1:(ncol(raw_data) - 1)])
  target <- as.vector(unlist((raw_data[,(ncol(raw_data))])))
  
  if (nrow(totals) > 1) {
    E <- as.matrix(totals[,1:(ncol(totals) - 1)])
    F <- as.vector(unlist(totals[,(ncol(totals))]))
  } else if (nrow(totals) == 1) {
    E <- as.vector(unlist(totals[,1:(ncol(totals) - 1)]))
    F <- as.vector(unlist(totals[,(ncol(totals))]))
  }
  
  G<-as.matrix(constraints[,1:(ncol(constraints) - 1)])
  H <- as.vector(constraints[,(ncol(constraints))])
  
  Wx<-as.vector(unlist(weights_x))
  Wa<-as.vector(unlist(weights_a))

  X <- lsei(A=par_mat, B=target, E=E, F=F, G=G, H=H, Wx=Wx, Wa=Wa)
  tmp_x<-t(X$X)
  rownames(tmp_x)<-"Component Proportions"
  res_props <- par_mat %*% matrix(data = X$X,ncol = 1)
  colnames(res_props)<-"Resulting Properties"
  
  return(list(renderTable(as.data.frame(tmp_x)),renderTable(as.data.frame(res_props))))
}

# par_mat<-as.matrix(raw_data[,1:(ncol(raw_data)-1)])
# target<-as.matrix(raw_data[,(ncol(raw_data))])
#
# #pro_mat2<-qr.solve(par_mat,target)  # requires MASS, can use ginv also (solve will work if matrix square)
#
# #solve(par_mat,target) # solves unconstrained problem
#
# #To set sum of components = 1, need to provide E and F such that Ex=F. i.e. pro1*comp1 + pro2*comp2 = 1
#
# E<-rep(1,ncol(par_mat))
# F<-1
#
# #Now set limits on components - i.e. all components >= 0. Need inequalities Gx>=H, but dealing with components individually so require diagonal matrix, then vector of component constraint. In this case all zeros, but can treat each component separately - e.g. force some > 0, others > 0.2 etc with H<-c(0,0,0.2,0,0.2,0.3 ...). Then can also add constraints such as all UK material less than 0.5, by appending a row in G [-1,-1,-1,0,0,0] and a value in H [-0.5] - or wherever you need to place the 1s. Alternatively could define all local > 0.5 by e.g. G [0,0,0,1,1,1] and H [0.5]
#
# G<-diag(ncol(par_mat))
# H<-rep(0,ncol(par_mat))
#
# # If you also want a maximum proportion of a component, then you need a negative diagonal matrix appended, with a negative H e.g. the following forces all components below 0.4 (as well as above 0)
# # G<-rbind(G,-G)
# # H<-c(H,rep(-0.4,ncol(par_mat)))
#
# # Make the solution given the parameter matrix, target, and constraints
#
# X <- lsei(par_mat, target, E, F, G, H)
# X
# res_props<-par_mat%*%matrix(data=X$X,ncol=1)
# res_props
#
# ## Can use Wx (vector=number of components - columns in par_mat) - with weights which components more "likely" to be chosen) and Wa (vector=number of parameters - number of rows in A - weighting which parameters are most important) vectors to adjust weighting of requirements
