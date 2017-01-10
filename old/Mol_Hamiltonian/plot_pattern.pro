pro plot_pattern, file

	NPat = 0
	openr,2,file
	readf,2,NPat
	pattern = dblarr(4,NPat)
	readf,2,pattern
	close,2

	Mup = pattern(0,*)
	Mlow = pattern(1,*)
	deltaM = Mup-Mlow
	split = pattern(2,*) * 1.d3
	stren = pattern(3,*)

; Normalize strengths
	for i = -1, 1 do begin
		ind = where(deltaM eq i)
		stren(ind) = stren(ind) / max(stren(ind))
	endfor

	xmin = min(split)
	xmax = max(split)
	deltax = xmax - xmin

	plot,[0,0],/nodata,xran=[xmin-0.3*deltax,xmax+0.3*deltax],yran=[-2.4,1.2],xsty=1,ysty=1,$
		xtit='Splitting [m!3�!6]',ytit='Relative strength'

	xyouts,xmin-0.2*deltax,0.2,'!7D!6M=1'
	xyouts,xmax+0.0*deltax,0.2,'!7D!6M=-1'
	xyouts,xmin-0.2*deltax,-1.8,'!7D!6M=0'


	yorigin = [0.0,-2.0,0.0]
	yorientation = [-1.0,1.0,1.0]

	very,0,line=1
	very,-2,line=1


; Plot components
	for i = -1, 1 do begin
		ind = where(deltaM eq i)
		for j = 0, n_elements(ind)-1 do begin
			plots, [split(ind(j)),split(ind(j))], $
				[yorigin(i+1),yorigin(i+1)+yorientation(i+1)*stren(ind(j))]
		endfor
	endfor
end
