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
	printf,2,'N: ', upper_quantum(0,2)

	printf,2,'Brot: ', upper_constants(0,0)
	printf,2,'Drot: ', upper_constants(0,1)
	printf,2,'Hrot: ', upper_constants(0,2)
	printf,2,'gamma_spin: ', upper_constants(0,3)
	printf,2,'A: ', upper_constants(0,4)
	printf,2,'A_D: ', upper_constants(0,5)
	printf,2,'gL: ', upper_constants(0,6)
	printf,2,'gS: ', upper_constants(0,7)
	printf,2,'gr: ', upper_constants(0,8)

	printf,2

	printf,2,'-----------'
	printf,2,'Lower level'
	printf,2,'-----------'

	printf,2,'Lambda: ', lower_quantum(0,0)
	printf,2,'S: ', lower_quantum(0,1)
	printf,2,'J: ', lower_quantum(0,3)
	printf,2,'N: ', lower_quantum(0,2)

	printf,2,'Brot: ', lower_constants(0,0)
	printf,2,'Drot: ', lower_constants(0,1)
	printf,2,'Hrot: ', lower_constants(0,2)
	printf,2,'gamma_spin: ', lower_constants(0,3)
	printf,2,'A: ', lower_constants(0,4)
	printf,2,'A_D: ', lower_constants(0,5)
	printf,2,'gL: ', lower_constants(0,6)
	printf,2,'gS: ', lower_constants(0,7)
	printf,2,'gr: ', lower_constants(0,8)

	close,2

	print, 'Configuration files generated...'
	print, 'Calling external program...'
	print, 'Diagonalizing hamiltonian...'

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

	upper_constants = dblarr(1,9)
	lower_constants = dblarr(1,9)
	upper_quantum = dblarr(1,4)
	lower_quantum = dblarr(1,4)
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