;
; IDL Widget Interface Procedures. This Code is automatically
;     generated and should not be modified.

;
; Generated on:	12/03/2004 23:19.52
;
pro WID_BASE_0_event, Event

  wWidget =  Event.top

  case Event.id of
	Widget_Info(wWidget, FIND_BY_UNAME='ZEEMAN_PATTERN'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	CalculateZeeman, Event
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='PLOT_PATTERN'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	PlotPattern, Event
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='PLOT_PATTERN_PS'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	PlotPatternPS, Event
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='LOAD_TRANSITION'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	LoadTransition, Event
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='SAVE_TRANSITION'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	SaveTransition, Event
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='WAVELENGTH_UNITS'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	if (Event.Select eq 1) then ChangeUnits, Event, 0
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='ENERGY_UNITS'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	if (Event.Select eq 1) then ChangeUnits, Event, 1
        endif
    end

	Widget_Info(wWidget, FIND_BY_UNAME='FREQUENCY_UNITS'): begin
    	if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' ) then begin
        	if (Event.Select eq 1) then ChangeUnits, Event, 2
        endif
    end

	else:
  endcase

end

pro WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_

  close, /all

  Resolve_Routine, 'hamilt_eventcb',/COMPILE_FULL_FILE  ; Load event callback routines

  state = {WID_BASE_0: 0L, WID_BASE_1: 0L, WID_BASE_2: 0L, WID_BASE_3: 0L, $
  	WID_BASE_6: 0L, WID_BASE_7: 0L, WID_TABLE_0: 0L, WID_TABLE_1: 0L, $
  	WID_TABLE_2: 0L, WID_TABLE_3: 0L, WID_TABLE_4: 0L, WID_TABLE_5: 0L, WID_SLIDER_0 : 0L, $
  	WID_SLIDER_1 : 0L, WID_BUTTON_0 : 0L, WID_BUTTON_1 : 0L, WID_BUTTON_2 : 0L, WID_BUTTON_3 : 0L,$
  	WID_BUTTON_4 : 0L, WID_TEXT_0 : 0L, WID_TEXT_1 : 0L, WID_TEXT_2 : 0L, WID_TEXT_3 : 0L, $
  	WID_TEXT_4 : 0L, Pattern : dblarr(4,2), WID_BUTTON_5 : 0L, WID_BUTTON_6 : 0L, WID_BUTTON_7 : 0L,$
  	PLOT_UNITS : ''}

  state.WID_BASE_0 = Widget_Base( GROUP_LEADER=wGroup, UNAME='WID_BASE_0'  $
      ,XOFFSET=5 ,YOFFSET=5 ,SCR_XSIZE=863 ,SCR_YSIZE=558  $
      ,TITLE='Molecular Hamiltonian GUI' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(state.WID_BASE_0, UNAME='WID_LABEL_0'  $
      ,XOFFSET=17 ,YOFFSET=12 ,SCR_XSIZE=129 ,SCR_YSIZE=12  $
      ,/ALIGN_LEFT ,VALUE='Upper level constants')


  state.WID_BASE_1 = Widget_Base(state.WID_BASE_0, UNAME='WID_BASE_1' ,XOFFSET=8  $
      ,YOFFSET=26 ,SCR_XSIZE=315 ,SCR_YSIZE=265 ,TITLE='IDL' ,SPACE=3  $
      ,XPAD=3 ,YPAD=3)

  init_constants = fltarr(1,10)
  init_constants(0,7) = 1.0
  init_constants(0,8) = 2.0
  init_quantum = fltarr(1,5)


  state.WID_TABLE_0 = Widget_Table(state.WID_BASE_1, UNAME='WID_TABLE_0'  $
      ,XOFFSET=3 ,YOFFSET=3 ,/EDITABLE ,COLUMN_LABELS=[ 'Constant' ]  $
      ,ROW_LABELS=[ 'B_rot', 'D_rot', 'H_rot', 'gamma', 'lambda_ss', 'A', 'A_D',  $
      'g_l', 'g_s', 'g_rot' ] ,XSIZE=1 ,YSIZE=10, VALUE = init_constants)


  state.WID_TABLE_1 = Widget_Table(state.WID_BASE_1, UNAME='WID_TABLE_1'  $
      ,XOFFSET=160 ,YOFFSET=3 ,/EDITABLE, COLUMN_LABELS=[ 'Value' ]  $
      ,ROW_LABELS=[ 'Lambda', 'S', 'i', 'J', 'par' ] ,XSIZE=1 ,YSIZE=5, VALUE=init_quantum)


  state.WID_SLIDER_0 = Widget_Slider(state.WID_BASE_1, UNAME='WID_SLIDER_0'  $
      ,XOFFSET=275 ,YOFFSET=133 ,SCR_XSIZE=28 ,SCR_YSIZE=26  $
      ,/VERTICAL ,MAXIMUM=2 ,VALUE=1)


 ; state.WID_TEXT_0 = Widget_Text(state.WID_BASE_1, UNAME='WID_TEXT_0' ,XOFFSET=95  $
 ;     ,YOFFSET=212 ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,XSIZE=20 ,YSIZE=1)


  WID_LABEL_1 = Widget_Label(state.WID_BASE_1, UNAME='WID_LABEL_1'  $
      ,XOFFSET=236 ,YOFFSET=140 ,SCR_XSIZE=35 ,SCR_YSIZE=20  $
      ,/ALIGN_LEFT ,VALUE='Delta J')


;  WID_LABEL_3 = Widget_Label(state.WID_BASE_1, UNAME='WID_LABEL_3'  $
;      ,XOFFSET=10 ,YOFFSET=214 ,/ALIGN_LEFT ,VALUE='Hamiltonian'+ $
;      ' size')


  WID_LABEL_2 = Widget_Label(state.WID_BASE_0, UNAME='WID_LABEL_2'  $
      ,XOFFSET=170 ,YOFFSET=12 ,/ALIGN_LEFT ,VALUE='Upper level'+ $
      ' quantum numbers')


  state.WID_BASE_2 = Widget_Base(state.WID_BASE_0, UNAME='WID_BASE_2'  $
      ,XOFFSET=327 ,YOFFSET=26 ,SCR_XSIZE=316 ,SCR_YSIZE=265  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  state.WID_TABLE_2 = Widget_Table(state.WID_BASE_2, UNAME='WID_TABLE_2'  $
      ,XOFFSET=4 ,YOFFSET=3 ,/EDITABLE ,COLUMN_LABELS=[ 'Constants' ]  $
      ,ROW_LABELS=[ 'B_rot', 'D_rot', 'H_rot', 'gamma', 'lambda_ss', 'A', 'A_D',  $
      'g_l', 'g_s', 'g_rot' ] ,XSIZE=1 ,YSIZE=10, VALUE = init_constants)


  state.WID_TABLE_3 = Widget_Table(state.WID_BASE_2, UNAME='WID_TABLE_3'  $
      ,XOFFSET=160 ,YOFFSET=4 ,/EDITABLE, COLUMN_LABELS=[ 'Value' ]  $
      ,ROW_LABELS=[ 'Lambda', 'S', 'i', 'J', 'par'] ,XSIZE=1 ,YSIZE=5, VALUE=init_quantum)


  state.WID_SLIDER_1 = Widget_Slider(state.WID_BASE_2, UNAME='WID_SLIDER_1'  $
      ,XOFFSET=270 ,YOFFSET=133 ,SCR_XSIZE=28 ,SCR_YSIZE=26  $
      ,/VERTICAL ,MAXIMUM=2 ,VALUE=1)


 ; state.WID_TEXT_1 = Widget_Text(state.WID_BASE_2, UNAME='WID_TEXT_1' ,XOFFSET=97  $
 ;     ,YOFFSET=212 ,SCR_XSIZE=62 ,SCR_YSIZE=18 ,XSIZE=20 ,YSIZE=1)


  WID_LABEL_4 = Widget_Label(state.WID_BASE_2, UNAME='WID_LABEL_4'  $
      ,XOFFSET=230 ,YOFFSET=140 ,/ALIGN_LEFT ,VALUE='Delta J')


;  WID_LABEL_5 = Widget_Label(state.WID_BASE_2, UNAME='WID_LABEL_5'  $
;      ,XOFFSET=9 ,YOFFSET=214 ,/ALIGN_LEFT ,VALUE='Hamiltonian size')


  WID_LABEL_6 = Widget_Label(state.WID_BASE_0, UNAME='WID_LABEL_6'  $
      ,XOFFSET=492 ,YOFFSET=12 ,/ALIGN_LEFT ,VALUE='Lower level'+ $
      ' quantum numbers')


  WID_LABEL_7 = Widget_Label(state.WID_BASE_0, UNAME='WID_LABEL_7'  $
      ,XOFFSET=345 ,YOFFSET=12 ,/ALIGN_LEFT ,VALUE='Lower level'+ $
      ' constants')


  state.WID_BASE_3 = Widget_Base(state.WID_BASE_0, UNAME='WID_BASE_3'  $
      ,XOFFSET=649 ,YOFFSET=6 ,SCR_XSIZE=145 ,SCR_YSIZE=296  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  state.WID_TEXT_2 = Widget_Text(state.WID_BASE_3, UNAME='WID_TEXT_2' ,XOFFSET=21  $
      ,YOFFSET=100 ,SCR_XSIZE=94 ,SCR_YSIZE=19 ,/EDITABLE ,XSIZE=20  $
      ,YSIZE=1, VALUE='0')

  state.WID_TEXT_3 = Widget_Text(state.WID_BASE_3, UNAME='WID_TEXT_3' ,XOFFSET=21  $
      ,YOFFSET=140 ,SCR_XSIZE=94 ,SCR_YSIZE=19 ,/EDITABLE ,XSIZE=20  $
      ,YSIZE=1, VALUE='0')


  WID_LABEL_8 = Widget_Label(state.WID_BASE_3, UNAME='WID_LABEL_8'  $
      ,XOFFSET=21 ,YOFFSET=83 ,/ALIGN_LEFT ,VALUE='Magnetic field')

  WID_LABEL_12 = Widget_Label(state.WID_BASE_3, UNAME='WID_LABEL_12'  $
      ,XOFFSET=21 ,YOFFSET=123 ,/ALIGN_LEFT ,VALUE='Wavelength')


  WID_LABEL_9 = Widget_Label(state.WID_BASE_3, UNAME='WID_LABEL_9'  $
      ,XOFFSET=122 ,YOFFSET=102 ,/ALIGN_LEFT ,VALUE='G')

  WID_LABEL_13 = Widget_Label(state.WID_BASE_3, UNAME='WID_LABEL_13'  $
      ,XOFFSET=122 ,YOFFSET=142 ,/ALIGN_LEFT ,VALUE='Â')

  WID_LABEL_14 = Widget_Label(state.WID_BASE_3, UNAME='WID_LABEL_14'  $
      ,XOFFSET=20 ,YOFFSET=260 ,/ALIGN_LEFT ,VALUE='Line name')

  state.WID_TEXT_4 = Widget_Text(state.WID_BASE_3, UNAME='WID_TEXT_4' ,XOFFSET=21  $
      ,YOFFSET=275 ,SCR_XSIZE=94 ,SCR_YSIZE=19 ,/EDITABLE ,XSIZE=20  $
      ,YSIZE=1, VALUE='')

  state.WID_BUTTON_0 = Widget_Button(state.WID_BASE_3, UNAME='LOAD_TRANSITION'  $
      ,XOFFSET=15 ,YOFFSET=18 ,SCR_XSIZE=109 ,SCR_YSIZE=29  $
      ,/ALIGN_CENTER ,VALUE='Load transition')

  state.WID_BUTTON_1 = Widget_Button(state.WID_BASE_3, UNAME='SAVE_TRANSITION'  $
      ,XOFFSET=15 ,YOFFSET=48 ,SCR_XSIZE=109 ,SCR_YSIZE=29  $
      ,/ALIGN_CENTER ,VALUE='Save transition')

  state.WID_BUTTON_2 = Widget_Button(state.WID_BASE_3, UNAME='ZEEMAN_PATTERN'  $
      ,XOFFSET=15 ,YOFFSET=168 ,SCR_XSIZE=109 ,SCR_YSIZE=29  $
      ,/ALIGN_CENTER ,VALUE='Zeeman Pattern')

  state.WID_BUTTON_3 = Widget_Button(state.WID_BASE_3, UNAME='PLOT_PATTERN'  $
      ,XOFFSET=15 ,YOFFSET=198 ,SCR_XSIZE=109 ,SCR_YSIZE=29  $
      ,/ALIGN_CENTER ,VALUE='Plot Pattern')

  state.WID_BUTTON_4 = Widget_Button(state.WID_BASE_3, UNAME='PLOT_PATTERN_PS'  $
      ,XOFFSET=15 ,YOFFSET=228 ,SCR_XSIZE=109 ,SCR_YSIZE=29  $
      ,/ALIGN_CENTER ,VALUE='Plot Pattern PS')


  state.WID_BASE_6 = Widget_Base(state.WID_BASE_0, UNAME='WID_BASE_6' ,XOFFSET=8  $
      ,YOFFSET=295 ,SCR_XSIZE=405 ,SCR_YSIZE=212 ,TITLE='IDL'  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3)


  state.WID_TABLE_4 = Widget_Table(state.WID_BASE_6, UNAME='WID_TABLE_4'  $
      ,YOFFSET=18 ,COLUMN_LABELS=[ 'Energy', 'Splitting', 'g_lande',  $
      'g_caseb' ] ,XSIZE=4 ,YSIZE=8, VALUE=fltarr(4,8), COLUMN_WIDTHS=80, UVALUE=dblarr(4,8))


  WID_LABEL_10 = Widget_Label(state.WID_BASE_6, UNAME='WID_LABEL_10'  $
      ,XOFFSET=4 ,YOFFSET=2 ,/ALIGN_LEFT ,VALUE='Upper level'+ $
      ' eigenvalues')


  state.WID_BASE_7 = Widget_Base(state.WID_BASE_0, UNAME='WID_BASE_7'  $
      ,XOFFSET=420 ,YOFFSET=296 ,SCR_XSIZE=405 ,SCR_YSIZE=212  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  state.WID_TABLE_5 = Widget_Table(state.WID_BASE_7, UNAME='WID_TABLE_5'  $
      ,YOFFSET=19 ,COLUMN_LABELS=[ 'Energy', 'Splitting', 'g_lande',  $
      'g_caseb' ] ,XSIZE=4 ,YSIZE=8, VALUE=fltarr(4,8), COLUMN_WIDTHS=80)


  WID_LABEL_11 = Widget_Label(state.WID_BASE_7, UNAME='WID_LABEL_11'  $
      ,XOFFSET=4 ,YOFFSET=2 ,/ALIGN_LEFT ,VALUE='Lower level'+ $
      ' eigenvalues')

  WID_BASE_4 = Widget_Base(state.WID_BASE_2, UNAME='WID_BASE_4' ,XOFFSET=196  $
      ,YOFFSET=170 ,TITLE='IDL' ,COLUMN=1 ,/EXCLUSIVE)


  state.WID_BUTTON_5 = Widget_Button(WID_BASE_4, UNAME='WAVELENGTH_UNITS'  $
      ,/ALIGN_LEFT ,VALUE='Wavelength units')

  state.WID_BUTTON_6 = Widget_Button(WID_BASE_4, UNAME='ENERGY_UNITS'  $
      ,/ALIGN_LEFT ,VALUE='Energy units')

  state.WID_BUTTON_7 = Widget_Button(WID_BASE_4, UNAME='FREQUENCY_UNITS'  $
      ,/ALIGN_LEFT ,VALUE='Frequency units')


  widget_control, state.WID_BASE_0, SET_UVALUE=state

  Widget_Control, /REALIZE, state.WID_BASE_0

  XManager, 'WID_BASE_0', state.WID_BASE_0, /NO_BLOCK

end
;
; Empty stub procedure used for autoloading.
;
pro hamilt, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
  WID_BASE_0, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_
end
