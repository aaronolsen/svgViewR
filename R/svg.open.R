svg.open <- function(file = NULL, window.title="svgViewR", animate.duration = 1, 
	animate.speed = 1, interpolate = TRUE, timeline = TRUE, 
	mode = 'webgl', animate.reverse = FALSE, animate.repeat = -1, 
	margin = 20, col = "white", times = NULL, clock = FALSE, stats = FALSE, panel = FALSE, 
	show.control = TRUE, start.rotate = TRUE, rotate.speed = 1.2, camera.near = 0.01, fov = 45,
	zoom.speed = 1, pan.speed = 0.2, layers = NULL, connection = TRUE, 
	close.on.done = TRUE, file.type = NULL, app.dir.src = NULL, debug = FALSE, 
	src.link = NULL){

	svg.new(file, window.title=window.title, animate.duration=animate.duration, 
		animate.speed=animate.speed, interpolate=interpolate, timeline=timeline, 
		mode=mode, animate.reverse=animate.reverse, animate.repeat=animate.repeat, 
		margin=margin, col=col, times=times, clock=clock, stats=stats, panel=panel, 
		show.control=show.control, start.rotate=start.rotate, rotate.speed=rotate.speed, camera.near=camera.near,
		fov=fov, zoom.speed=zoom.speed, pan.speed=pan.speed, layers=layers, connection=connection, 
		close.on.done=close.on.done, file.type=file.type, app.dir.src=app.dir.src, debug=debug, 
		src.link=src.link)
}