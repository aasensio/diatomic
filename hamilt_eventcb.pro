;
; IDL Event Callback Procedures
; hamilt_eventcb
;
; Generated on:	12/03/2004 18:05.27
;
;
; Empty stub procedure used for autoloading.
;
pro hamilt_eventcb
end


pro ChangeUnits, Event, which
	widget_control, Event.top, GET_UVALUE=state
	if (which eq 0) then begin
		state.PLOT_UNITS = 'WAVELENGTH'
	endif

	if (which eq 1) then begin
		state.PLOT_UNITS = 'ENERGY'
	endif

	if (which eq 2) then begin
		state.PLOT_UNITS = 'FREQUENCY'
	endif

	widget_control, Event.top, SET_UVALUE=state
end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro CalculateZeeman, Event
	widget_control, Event.top, GET_UVALUE=state

; Get the constants of the upper level
	widget_control, state.WID_TABLE_0, GET_VALUE=upper_constants
; Get the constants of the lower level
	widget_control, state.WID_TABLE_2, GET_VALUE=lower_constants

; Get the quantum numbers of the upper level
	widget_control, state.WID_TABLE_1, GET_VALUE=upper_quantum
; Get the quantum numbers of the lower level
	widget_control, state.WID_TABLE_3, GET_VALUE=lower_quantum

; Get DeltaJ of the upper level
	widget_control, state.WID_SLIDER_0, GET_VALUE=upper_deltaJ
; Get DeltaJ of the lower level
	widget_control, state.WID_SLIDER_1, GET_VALUE=lower_deltaJ

; Get the wavelength
	widget_control, state.WID_TEXT_3, GET_VALUE=wavelength
; Get the magnetic field strength
	widget_control, state.WID_TEXT_2, GET_VALUE=magnetic_field

; Now save the configuration files for the FORTRAN code
	openw,2,'config.dat'
	printf,2,'#'
	printf,2,'#'
	printf,2,'#'
	printf,2,'#'
	printf,2,'trans_from_idl.dat'
	printf,2
	printf,2,'#'
	printf,2,magnetic_field
	close,2

	openw,2,'trans_from_idl.dat'
	printf,2,'Wavelength: ', wavelength
	printf,2,'-----------'
	printf,2,'Upper level'
	printf,2,'-----------'

	printf,2,'Lambda: ', upper_quantum(0,0)
	printf,2,'S: ', upper_quantum(0,1)
	printf,2,'J: ', upper_quantum(0,3)
	printf,2,'i: ', upper_quantum(0,2)
	printf,2,'par: ', upper_quantum(0,4)

	printf,2,'Brot: ', upper_constants(0,0)
	printf,2,'Drot: ', upper_constants(0,1)
	printf,2,'Hrot: ', upper_constants(0,2)
	printf,2,'gamma_spin: ', upper_constants(0,3)
	printf,2,'lambda_ss: ', upper_constants(0,4)
	printf,2,'A: ', upper_constants(0,5)
	printf,2,'A_D: ', upper_constants(0,6)
	printf,2,'gL: ', upper_constants(0,7)
	printf,2,'gS: ', upper_constants(0,8)
	printf,2,'gr: ', upper_constants(0,9)

	printf,2

	printf,2,'-----------'
	printf,2,'Lower level'
	printf,2,'-----------'

	printf,2,'Lambda: ', lower_quantum(0,0)
	printf,2,'S: ', lower_quantum(0,1)
	printf,2,'J: ', lower_quantum(0,3)
	printf,2,'i: ', lower_quantum(0,2)
	printf,2,'par: ', lower_quantum(0,4)

	printf,2,'Brot: ', lower_constants(0,0)
	printf,2,'Drot: ', lower_constants(0,1)
	printf,2,'Hrot: ', lower_constants(0,2)
	printf,2,'gamma_spin: ', lower_constants(0,3)
	printf,2,'lambda_ss: ', lower_constants(0,4)
	printf,2,'A: ', lower_constants(0,5)
	printf,2,'A_D: ', lower_constants(0,6)
	printf,2,'gL: ', lower_constants(0,7)
	printf,2,'gS: ', lower_constants(0,8)
	printf,2,'gr: ', lower_constants(0,9)

	close,2

	print, 'Configuration files generated...'
	print, 'Calling external program...'
	print, 'Diagonalizing hamiltonian...'

	spawn, './diatomic'

	Jup = upper_quantum(0,3)
	Jlow = lower_quantum(0,3)
	Nup = 2*Jup+1
	Nlow = 2*Jlow+1
	data_up = dblarr(5,Nup)
	data_low = dblarr(5,Nlow)

	openr,2,'fort.20'
	readf,2,data_up
	readf,2,data_low
	close,2

	Mup = string(data_up(0,*),FORMAT='(F5.1)')
	Eup = data_up(1,*)
	DeltaEUp = data_up(2,*)
	landeUp = data_up(3,*)
	lande_bUp = data_up(4,*)

	Mlow = string(data_low(0,*),FORMAT='(F5.1)')
	ELow = data_low(1,*)
	DeltaELow = data_low(2,*)
	landeLow = data_low(3,*)
	lande_bLow = data_low(4,*)


; Set the upper level results
	widget_control, state.WID_TABLE_4, YSIZE=Nup
	widget_control, state.WID_TABLE_4, ROW_LABELS=Mup
	widget_control, state.WID_TABLE_4, SET_VALUE=[EUp,DeltaEUp,landeUp,lande_bUp]

; Set the lower level results
	widget_control, state.WID_TABLE_5, YSIZE=Nlow
	widget_control, state.WID_TABLE_5, ROW_LABELS=Mlow
	widget_control, state.WID_TABLE_5, SET_VALUE=[ELow,DeltaELow,landeLow,lande_bLow]

; Read the Zeeman pattern and save it in the UVALUE of TABLE 4
	NPat = 0
	openr,2,'pattern.dat'
	readf,2,NPat
	pattern = dblarr(4,NPat)
	readf,2,pattern
	close,2

	widget_control,state.WID_TABLE_4, SET_UVALUE=pattern

end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro PlotPattern, Event
	widget_control, Event.top, GET_UVALUE=state

	widget_control, state.WID_TABLE_4, GET_UVALUE=pattern

	widget_control, state.WID_TEXT_4, GET_VALUE=tit

	widget_control, state.WID_TEXT_3, GET_VALUE=wave0_str
	wave0 = float(wave0_str(0))

	Mup = pattern(0,*)
	Mlow = pattern(1,*)
	deltaM = Mup-Mlow
	split = pattern(2,*) * 1.d3
	xtit = 'Splitting [m!3Â!6]'

	if (state.PLOT_UNITS eq 'FREQUENCY') then begin
		split = -3.d10 / ((wave0*1.d-8)^2) * pattern(2,*)*1.d-8
		xtit = 'Splitting [Hz]'
	endif

	if (state.PLOT_UNITS eq 'ENERGY') then begin
		split = -(pattern(2,*)*1.d-8) / (wave0*1.d-8)^2
		xtit = 'Splitting [cm!e-1!n]'
	endif

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
		xtitle=xtit,ytit='Relative strength',title=tit(0)

	xyouts,xmin-0.2*deltax,-0.2,'!7D!6M=1'
	xyouts,xmax+0.0*deltax,0.2,'!7D!6M=-1'
	xyouts,xmin-0.2*deltax,-1.8,'!7D!6M=0'


; DeltaM=Mu-Ml
; DeltaM=1 -> going down in the upper part
; DeltaM=0 -> going up in the lower part
; DeltaM=-1 -> going up in the upper part
	yorigin = [0.0,-2.0,0.0]
	yorientation = [1.0,1.0,-1.0]

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

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro PlotPatternPS, Event
	file = dialog_pickfile(filter = '*.eps', /write)
	abre_ps,file,/encapsulated
	PlotPattern, Event
	cierra_ps
end


;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro LoadTransition, Event
	widget_control, Event.top, GET_UVALUE=state

	upper_constants = dblarr(1,10)
	lower_constants = dblarr(1,10)
	upper_quantum = dblarr(1,5)
	lower_quantum = dblarr(1,5)
	upper_deltaJ = 1.d0
	lower_deltaJ = 1.d0
	wavelength = 0.d0
	magnetic_field = 0.d0

	file = dialog_pickfile(filter = '*.transition', /read)

	openr,2,file
	readf,2,upper_constants
	readf,2,lower_constants
	readf,2,upper_quantum
	readf,2,lower_quantum
	readf,2,upper_deltaJ
	readf,2,lower_deltaJ
	readf,2,wavelength
	readf,2,magnetic_field
	close,2

; Set the constants of the upper level
	widget_control, state.WID_TABLE_0, SET_VALUE=upper_constants
; Set the constants of the lower level
	widget_control, state.WID_TABLE_2, SET_VALUE=lower_constants

; Set the quantum numbers of the upper level
	widget_control, state.WID_TABLE_1, SET_VALUE=upper_quantum
; Set the quantum numbers of the lower level
	widget_control, state.WID_TABLE_3, SET_VALUE=lower_quantum

; Set DeltaJ of the upper level
	widget_control, state.WID_SLIDER_0, SET_VALUE=upper_deltaJ
; Set DeltaJ of the lower level
	widget_control, state.WID_SLIDER_1, SET_VALUE=lower_deltaJ

; Set the wavelength
	widget_control, state.WID_TEXT_3, SET_VALUE=strtrim(string(wavelength),2)

; Set the magnetic field
	widget_control, state.WID_TEXT_2, SET_VALUE=strtrim(string(magnetic_field),2)

	print, 'Transition loaded from the file : ', file

end

;-----------------------------------------------------------------
; Activate Button Callback Procedure.
; Argument:
;   Event structure:
;
;   {WIDGET_BUTTON, ID:0L, TOP:0L, HANDLER:0L, SELECT:0}
;
;   ID is the widget ID of the component generating the event. TOP is
;       the widget ID of the top level widget containing ID. HANDLER
;       contains the widget ID of the widget associated with the
;       handler routine.

;   SELECT is set to 1 if the button was set, and 0 if released.
;       Normal buttons do not generate events when released, so
;       SELECT will always be 1. However, toggle buttons (created by
;       parenting a button to an exclusive or non-exclusive base)
;       return separate events for the set and release actions.

;   Retrieve the IDs of other widgets in the widget hierarchy using
;       id=widget_info(Event.top, FIND_BY_UNAME=name)

;-----------------------------------------------------------------
pro SaveTransition, Event
	widget_control, Event.top, GET_UVALUE=state

; Get the constants of the upper level
	widget_control, state.WID_TABLE_0, GET_VALUE=upper_constants
; Get the constants of the lower level
	widget_control, state.WID_TABLE_2, GET_VALUE=lower_constants

; Get the quantum numbers of the upper level
	widget_control, state.WID_TABLE_1, GET_VALUE=upper_quantum
; Get the quantum numbers of the lower level
	widget_control, state.WID_TABLE_3, GET_VALUE=lower_quantum

; Get DeltaJ of the upper level
	widget_control, state.WID_SLIDER_0, GET_VALUE=upper_deltaJ
; Get DeltaJ of the lower level
	widget_control, state.WID_SLIDER_1, GET_VALUE=lower_deltaJ

; Get the wavelength
	widget_control, state.WID_TEXT_3, GET_VALUE=wavelength

; Get the magnetic field
	widget_control, state.WID_TEXT_2, GET_VALUE=magnetic_field

	file = dialog_pickfile(filter = '*.transition', /write)

	openw,2,file
	printf,2,upper_constants
	printf,2,lower_constants
	printf,2,upper_quantum
	printf,2,lower_quantum
	printf,2,upper_deltaJ
	printf,2,lower_deltaJ
	printf,2,wavelength
	printf,2,magnetic_field
	close,2

	print, 'Transition saved in the file : ', file

end