svg.frame <- function(x = NULL, ranges = NULL, grid.lwd = 1, tick.lwd = 1, tick.num = 10, 
	tick.label.size = 'auto', axis.label = c('x', 'y', 'z'), axis.label.size = 'auto', 
	tick.label.opacity = 1, axis.label.opacity = 1, grid.opacity = 0.1, axis.col = rgb(0.5,0.5,0.5), 
	grid.col = rgb(0.8,0.8,0.8), text.col = 'black', z.index=0, lim.exact=FALSE, name = NULL, 
	file=NULL){

	svg_box <- svg.box(x=x, ranges=ranges, sides=1:3, grid.lwd=grid.lwd, tick.axes=c(2,3,2), 
		tick.labels=c(2,3,2), tick.lwd=tick.lwd, tick.num=tick.num, tick.label.size=tick.label.size, 
		tick.label.opacity=tick.label.opacity, axis.label=axis.label, axis.label.opacity=axis.label.opacity, 
		axis.label.size=axis.label.size, grid.opacity=grid.opacity, axis.col=axis.col, grid.col, 
		text.col=text.col, z.index=z.index, lim.exact=lim.exact, name=name, file=file)
	
	list('lim'=svg_box$lim)
}