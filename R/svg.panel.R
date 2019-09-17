svg.panel <- function(col='auto', bg.col='none'){
	
	# Get viewer background color
	vwr_bg_col <- webColor(svgviewr_env$js_var[['bg_col']], rev=TRUE)
	
	# Get brightness of color
	# 1: white
	# 0: black
	col_dark <- sum(col2rgb(vwr_bg_col)) / (255*3)
	
	if(col == 'auto'){
		if(col_dark > 0.5){
			col <- webColor('black')
		}else{
			col <- webColor('white')
		}
	}

	# Add panel properties
	svgviewr_env$js_var[['panel']] <- TRUE
	svgviewr_env$js_var[['panel_col']] <- col
	svgviewr_env$js_var[['panel_bg_col']] <- bg.col
}