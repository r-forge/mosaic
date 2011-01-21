require(Rcmdr);

.First.lib <- function(libname, pkgname){ 
		if (!interactive()) return() ;
		Rcmdr <- options()$Rcmdr ;
		plugins <- Rcmdr$plugins ;
		if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) { 
				Rcmdr$plugins <- c(plugins, pkgname) ;
				options(Rcmdr=Rcmdr) ;
				closeCommander(ask=FALSE, ask.save=TRUE) ;
				Commander() ;
		} 
} 


meansd <- function(x,na.rm=TRUE) {
    val <- c(mean(x,na.rm=na.rm),sd(x,na.rm=na.rm));
    names(val) <- c('mean','sd');
    return(val);
}

fvnum <- function(x,na.rm=TRUE) {
    qq <- quantile(x,na.rm=na.rm);
    val <- c(qq);
    names(val) <- c(names(qq));
    return(val);
}

favstats <- function(x,na.rm=TRUE) {
    qq <- quantile(x,na.rm=na.rm);
    val <- c(qq,mean(x,na.rm=na.rm),sd(x,na.rm=na.rm));
    names(val) <- c(names(qq),'mean','sd');
    return(val);
}

conditionBox <- defmacro(recall=NULL, label=gettextRcmdr("Condition by:"), 
				initialLabel=gettextRcmdr("Condition by"),
#    			plotLinesByGroup=FALSE, positionLegend=FALSE, 
#				plotLinesByGroupsText=gettextRcmdr("Plot lines by group"),
    expr={
        env <- environment()
        .cond <- FALSE
        .linesByGroup <- FALSE
        .condLabel <- tclVar(paste(initialLabel, "...", sep=""))
        .factors <- Factors()
        onCond <- function(){
            if (length(.factors) == 0){
                errorCondition(recall=recall, message=gettextRcmdr("There no factors in the active data set.")) 
                return()
                }
            initializeDialog(subdialog, title=gettextRcmdr("Conditioning Variable"))
            condBox <- variableListBox(subdialog, .factors, 
					title=gettextRcmdr("Conditioning variable (pick one)"))
            onOKsub <- function() {
                cond <- getSelection(condBox)
                if (length(cond) == 0){
                    assign(".cond", FALSE, envir=env)
                    tclvalue(.condLabel) <- paste(initialLabel, "...", sep="")
                    tkconfigure(condButton, fg="black")
                    if (GrabFocus()) tkgrab.release(subdialog)
                    tkdestroy(subdialog)
                    tkwm.deiconify(top)
                    if (GrabFocus()) tkgrab.set(top)
                    tkfocus(top)
                    tkwait.window(top)                
                    return()
                    }
                assign(".cond", cond, envir=env)
                tclvalue(.condLabel) <- paste(label, cond)
                tkconfigure(condButton, fg="blue")
                if (GrabFocus()) tkgrab.release(subdialog)
                tkdestroy(subdialog)
                tkwm.deiconify(top)
                if (GrabFocus()) tkgrab.set(top)
                tkfocus(top)
                tkwait.window(top)
                }
            subOKCancelHelp()
            tkgrid(getFrame(condBox), sticky="nw")
            tkgrid(subButtonsFrame, sticky="w")
            dialogSuffix(subdialog, onOK=onOKsub, rows=3,
				columns=2, focus=subdialog)
            }
        condFrame <- tkframe(top)
        condButton <- tkbutton(condFrame, textvariable=.condLabel, command=onCond, borderwidth=3)
        tkgrid(tklabel(condFrame, text="    "), condButton, sticky="w")
        })

xyPlot <- function(){
    require("lattice")
    initializeDialog(title=gettextRcmdr("Lattice xyplot"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("x-variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title=gettextRcmdr("y-variable (pick one)"))
    checkBoxes(frame="optionsFrame", 
        boxes=c("identify", "lsLine", "loess", "autokey"),
        initialValues=c(0, 0, 0, 0), 
        labels=gettextRcmdr(c("Identify points", "Least-squares line", 
            "Smooth Line", "Automatic legend")))
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    subsetBox()
    radioButtons(name="plotType",
      buttons=c("p", "l", "b"), 
      labels=c("Points", "Lines", "Both lines and points"), 
      title=gettextRcmdr("Plot Type"))
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        plotType <- tclvalue(plotTypeVariable)
        plotType <- trim.blanks(tclvalue(plotTypeVariable))
        #type <- gsub(" ","", type);
		print(plotType);
		typeChunk <- paste("type= \"", plotType, "\"", sep="");
		print(typeChunk);
        closeDialog()

        if (length(x) == 0 || length(y) == 0){
            errorCondition(recall=xyPlot, message=gettextRcmdr("You must select two variables"))
            return()
            }
        if (x == y) {
            errorCondition(recall=xyPlot, message=gettextRcmdr("x and y variables must be different"))
            return()
            }

        .activeDataSet <- ActiveDataSet()
        labels <- if("1" == tclvalue(identifyVariable)) 
            paste("rownames(", .activeDataSet, ")", sep="") else "FALSE"
#        box <- if ("1" == tclvalue(boxplotsVariable)) "'xy'" else "FALSE"
        identify <- if("1" == tclvalue(identifyVariable)) TRUE else FALSE
        add.reg.line <- if("1" == tclvalue(lsLineVariable)) TRUE else FALSE
        add.loess <- if ("1" == tclvalue(loessVariable)) TRUE else FALSE
        span <- as.numeric(tclvalue(sliderValue))
		if (add.loess || add.reg.line || identify) {
        	panelChunk <- "panel=function(x,y,...) {panel.xyplot(x,y,...);";
        	if (add.loess) panelChunk <- c(panelChunk, "panel.loess(x,y,span=", as.character(span/75), "); " );
        	if (add.reg.line) panelChunk <- c(panelChunk, " panel.lmline(x,y,...); " );
        	if (identify) panelChunk <- c(panelChunk, " panel.identify(x,y,...); " );
        	panelChunk <- c(panelChunk, "}");
        	panelChunk <- paste(panelChunk, collapse="");
		} else {
			panelChunk <- ""
		}

        subset <- tclvalue(subsetVariable)
        subsetChunk <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste("subset=", subset, sep="")

		groupsChunk <- if(.groups=='FALSE') "" else paste("groups=",.groups,sep="")

        tkdestroy(top)

        baseCommand <- paste("xyplot(", y, "~", x, ", data=", .activeDataSet, sep="");
        if (.cond == FALSE) { 
			baseCommand <- paste("xyplot(", y, "~", x, ", data=", .activeDataSet, sep=""); 
        } else { 
			baseCommand <- paste( "xyplot(", y, "~", x," | ", .cond, ", data=", .activeDataSet, sep=""); 
		}
		commandChunks <- c(baseCommand, groupsChunk, subsetChunk, typeChunk, panelChunk);
		commandChunks <- commandChunks[commandChunks != ""];
        commandLine <- paste(commandChunks, collapse=",");
		commandLine <- paste(commandLine, ")", sep="");
		print(commandChunks);
		print(commandLine);
        doItAndPrint(commandLine);
        activateMenus();
        tkfocus(CommanderWindow());
    }
    groupsBox(xyPlot, plotLinesByGroup=FALSE, positionLegend=FALSE)
    conditionBox(xyPlot)
    OKCancelHelp(helpSubject="xyplot")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Span for smooth")), slider, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(plotTypeFrame, columnspan=2, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(condFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=5, columns=2)
}

bwPlot <- function(){
    require("lattice")
    initializeDialog(title=gettextRcmdr("bwplot"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("quantitative variable (pick one)"))
    subsetBox()
    radioButtons(name="plotDirection",
      buttons=c("horizontal", "vertical"), 
      labels=c("Horizontal", "Vertical"), 
      title=gettextRcmdr("Box orientation (when groups given)"))
    onOK <- function(){
        x <- getSelection(xBox)
        plotDirection <- tclvalue(plotDirectionVariable)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=bwPlot, message=gettextRcmdr("You must select a quantitative variables"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
		groups <- if(.groups=='FALSE') "" else paste(.groups,sep="")
        tkdestroy(top)
        formula <-
            if (.groups == "FALSE" || plotDirection=="horizontal") 
                paste(groups,"~",x,sep="")
            else paste(x, "~",groups,sep="")
        if (.cond == FALSE) {
            doItAndPrint(paste("bwplot(", formula,
                ", data=", .activeDataSet, 
				subset, 
                ")", sep=""))
            }
        else {
            doItAndPrint(paste("bwplot(",formula," | ", .cond,
                ", data=", .activeDataSet, 
                subset, ")", sep=""))
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(bwPlot, plotLinesByGroup=FALSE, positionLegend=FALSE)
    conditionBox(bwPlot)
    OKCancelHelp(helpSubject="bwplot")
    tkgrid(getFrame(xBox), sticky="nw")    
    #tkgrid(optionsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(plotDirectionFrame, columnspan=2, sticky="w")
    tkgrid(condFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=5, columns=2)
    }


col.navy <- function() {

	darkBlue <- "#1C1C8E"
	lightBlue <- "#ACC7DD"
	veryLightBlue <- "#DDE8F1"
	paleGreen <- "#388638"

    list(background = list(col = "transparent"), 
        plot.polygon = list(col = lightBlue), 
        box.rectangle = list(col = darkBlue), 
        box.umbrella = list(col = darkBlue), 
        dot.line = list(col = "#e8e8e8"), 
        dot.symbol = list(col = darkBlue, pch=16), 
        plot.line = list(col = darkBlue, lwd=2), 
        plot.symbol = list(col = darkBlue,pch=16), 
        regions = list(col = heat.colors(100)), 
        reference.line = list(col = "#e8e8e8"), 
        add.line = list(lty=1, col = "gray40", lwd=2),
        superpose.line = list(lty=1:7, lwd=2,
                    col = c(darkBlue,"red","darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        superpose.symbol = list(pch = c(16,1, 3, 6, 0, 5, 17), 
            cex = rep(0.7, 7), 
            col = c(darkBlue,"red","darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        strip.background=list(alpha=1,
           col=c("#ffe5cc",veryLightBlue,"#ccffff",
                 "#cce6ff","#ffccff","#ffcccc","#ffffcc")
            ),
        strip.shingle=list(alpha=1,
            col = c("#ff7f00",darkBlue,"#00ffff",
                 "#0080ff","#ff00ff","#ff0000","#ffff00"))
        );
}




col.calvin <- function() {
	darkMaroon <- "#660000"
	lightMaroon <- "#993333"
	veryLightMaroon <- "#cc6666"
	darkBeige <- "#999966"
	lightBeige <- "#cccc99"
	veryLightBeige <- "#d7dac3"
	darkBlue <- "#003366"
	lightBlue <- "#336699"
	veryLightBlue <- "#6699cc"

    list(background = list(col = "transparent"), 
        plot.polygon = list(col = lightMaroon), 
        box.rectangle = list(col = darkMaroon), 
        box.umbrella = list(col = darkMaroon), 
        dot.line = list(col = "#e8e8e8"), 
        dot.symbol = list(col = darkMaroon, pch=16), 
        plot.line = list(col = darkMaroon, lwd=2), 
        plot.symbol = list(col = darkMaroon,pch=16), 
        regions = list(col = heat.colors(100)), 
        reference.line = list(col = "#e8e8e8"), 
        add.line = list(lty=1, col = "gray40", lwd=2),
        superpose.line = list(lty=1:7, lwd=2,
                    col = c(darkMaroon,lightBeige,"darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        superpose.symbol = list(pch = c(16,1, 3, 6, 0, 5, 17), 
            cex = rep(0.7, 7), 
            col = c(darkMaroon,lightBeige, "darkgreen","turquoise","orange",
                        "purple","pink","lightgreen")),
        strip.background=list(alpha=1,
           col=c("#ffe5cc",veryLightMaroon,"#ccffff",
                 "#cce6ff","#ffccff","#ffcccc","#ffffcc")
            ),
        strip.shingle=list(alpha=1,
            col = c("#ff7f00",darkMaroon,"#00ffff",
                 "#0080ff","#ff00ff","#ff0000","#ffff00"))
        );
}

latticeShowSettings <- function(){
	require(lattice)
    doItAndPrint(paste("show.settings()"))
}

latticeTheme <- function(){
    require("lattice");
    initializeDialog(title=gettextRcmdr("Select Lattice Theme"));
    onOK <- function(){
        themeString <- tclvalue(themeVariable)
		print(themeString);
        closeDialog()
        doItAndPrint(paste("trellis.par.set(theme=",themeString,"())",sep=""));
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="trellis.par.set")
    radioButtons(top,name="theme",
      buttons=c("colwhitebg", "colnavy", "colcalvin" ), 
      values=c("col.whitebg", "col.navy", "col.calvin" ), 
      labels=gettextRcmdr(c("col.whitebg", "Go Navy", "Go Calvin")), 
      title=gettextRcmdr("Select a lattice theme; select a lattice theme:"));
    tkgrid(themeFrame, columnspan=2, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=5, columns=3)
    }

latticeHistogram <- function(){
    require("lattice")
    initializeDialog(title=gettextRcmdr("Lattice histogram"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("quantitative variable (pick one)"))
    subsetBox()
    radioButtons(name="equalWidth",
      buttons=c("TRUE", "FALSE"), 
      labels=c("Equal Width", "Equal Area"), 
      title=gettextRcmdr("Bin style:"))
    radioButtons(name="type",
      buttons=c('density', 'probability','count'), 
      labels=c("Density", "Probability", "Count"), 
      title=gettextRcmdr("Type of histogram"))
    onOK <- function(){
        x <- getSelection(xBox)
        bins <- tclvalue(binsVariable)
        opts <- options(warn=-1)
        bins <- if (bins == gettextRcmdr("<auto>")) "" else 
            paste(", nint=",as.numeric(bins),sep="")
        options(opts)
        type <- trim.blanks(tclvalue(typeVariable))
        # type <- gsub(" ", "", type)
        type <- paste(", type=",'"',type,'"',sep="")
        equal.width <- tclvalue(equalWidthVariable)
		if (equal.width == 'FALSE') {
        	equal.width <- ", equal.widths=FALSE, breaks=NULL";
		} else {
        	equal.width <- "";
		}
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=latticeHistogram, 
              message=gettextRcmdr("You must select a quantitative variable"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        tkdestroy(top)
        formula <- paste("~",x,sep="");
        if (.cond == FALSE) {
            doItAndPrint(paste("histogram(", formula,
                ", data=", .activeDataSet, 
				subset, 
                type,
                bins,
                equal.width,
                ")", sep=""))
            }
        else {
            doItAndPrint(paste("histogram(",formula," | ", .cond,
                ", data=", .activeDataSet, 
                subset, 
                type,
                bins,
                equal.width,
                ")", sep=""))
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    conditionBox(latticeHistogram)
    OKCancelHelp(helpSubject="histogram")
    binsFrame <- tkframe(top)
    binsVariable <- tclVar(gettextRcmdr("<auto>"))
    binsField <- tkentry(binsFrame, width="6", textvariable=binsVariable)
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(tklabel(binsFrame, text=gettextRcmdr("Number of bins: ")), binsField, sticky="w")
    tkgrid(binsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(typeFrame, columnspan=2, sticky="w")
    tkgrid(equalWidthFrame, columnspan=2, sticky="w")
    tkgrid(condFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(binsField, sticky="e")
    dialogSuffix(rows=5, columns=2)
    }

HmiscStatisticsTable <- function(){
    require(Hmisc)
    initializeDialog(title=gettextRcmdr("Table of Statistics"))
    variablesFrame <- tkframe(top)
    groupBox <- variableListBox(variablesFrame, Factors(), selectmode="multiple", title=gettextRcmdr("Factors (pick one or more)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    radioButtons(name="method", buttons=c("response", "cross","reverse"), 
        labels=gettextRcmdr(c("response","cross","reverse")), title=gettextRcmdr("Method"))
    radioButtons(name="statistic", buttons=c("favstats", "meansd","fvnum"), 
        labels=gettextRcmdr(c("5 number; mean; standard deviation", "Mean & standard deviation", "5 number summary")), title=gettextRcmdr("Statistic(s)"))
    otherVariable <- tclVar("")
    otherButton <- tkradiobutton(statisticFrame, variable=statisticVariable, value="other")
    otherEntry <- tkentry(statisticFrame, width="20", textvariable=otherVariable)   
    tkgrid(tklabel(statisticFrame, text=gettextRcmdr("Other (specify)")), otherButton, otherEntry, sticky="w")
    onOK <- function(){
        groups <- getSelection(groupBox)
        if (0 == length(groups)) {
            errorCondition(recall=statisticsTable, message=gettextRcmdr("No factors selected."))
            return()
            }
        response <- getSelection(responseBox)
        if (0 == length(response)) {
            errorCondition(recall=statisticsTable, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        method <- tclvalue(methodVariable)
        statistic <- tclvalue(statisticVariable)
        if (statistic == "other") statistic <- tclvalue(otherVariable)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        groups.formula <- paste(groups, sep="+")
        print(groups);
        print(groups.formula);
        doItAndPrint(paste("summary(", response, "~", groups.formula,
            ", data=", .activeDataSet, 
            ", fun=", statistic,
            ", na.rm=TRUE",
            ', method="',method,'"',
            ")",
            sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="summary.formula")
    tkgrid(getFrame(groupBox), tklabel(variablesFrame, text="    "),
        getFrame(responseBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(statisticFrame, sticky="w")
    tkgrid(methodFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1, focus=otherEntry)
    }
