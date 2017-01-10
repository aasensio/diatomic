HEADER
; IDL Visual Widget Builder Resource file. Version 1
; Generated on:	12/04/2004 11:20.43
VERSION 1
END

WID_BASE_0 BASE 5 5 863 558
TLB
CAPTION "IDL"
XPAD = 3
YPAD = 3
SPACE = 3
BEGIN
  WID_LABEL_0 LABEL 7 10 129 12
  VALUE "Upper level constants"
  ALIGNLEFT
  END
  WID_BASE_1 BASE 8 26 315 265
  XPAD = 3
  YPAD = 3
  SPACE = 3
  BEGIN
    WID_TABLE_0 TABLE 3 3 0 0
    N_ROWS = 9
    N_COLS = 1
    NUMCOLLABELS = 1
    COLLABEL "Constant"
    NUMROWLABELS = 9
    ROWLABEL "B_rot"
    ROWLABEL "D_rot"
    ROWLABEL "H_rot"
    ROWLABEL "gamma"
    ROWLABEL "A"
    ROWLABEL "A_D"
    ROWLABEL "g_l"
    ROWLABEL "g_s"
    ROWLABEL "g_rot"
    EDITABLE
    ONINSERTCHAR "caca"
    END
    WID_TABLE_1 TABLE 156 3 0 0
    N_ROWS = 4
    N_COLS = 1
    NUMCOLLABELS = 1
    COLLABEL "Value"
    NUMROWLABELS = 4
    ROWLABEL "Lambda"
    ROWLABEL "S"
    ROWLABEL "N"
    ROWLABEL "J"
    END
    WID_SLIDER_0 SLIDER 275 124 28 26
    VALUE = 1
    MAXIMUM = 2
    VERTICAL
    END
    WID_TEXT_0 TEXT 95 212 62 18
    WIDTH = 20
    HEIGHT = 1
    END
    WID_LABEL_1 LABEL 236 130 35 20
    VALUE "Delta J"
    ALIGNLEFT
    END
    WID_LABEL_3 LABEL 10 214 0 0
    VALUE "Hamiltonian size"
    ALIGNLEFT
    END
  END
  WID_LABEL_2 LABEL 150 11 0 0
  VALUE "Upper level quantum numbers"
  ALIGNLEFT
  END
  WID_BASE_2 BASE 327 26 316 265
  XPAD = 3
  YPAD = 3
  SPACE = 3
  BEGIN
    WID_TABLE_2 TABLE 4 3 0 0
    N_ROWS = 9
    N_COLS = 1
    NUMCOLLABELS = 1
    COLLABEL "Constants"
    NUMROWLABELS = 9
    ROWLABEL "B_rot"
    ROWLABEL "D_rot"
    ROWLABEL "H_rot"
    ROWLABEL "gamma"
    ROWLABEL "A"
    ROWLABEL "A_D"
    ROWLABEL "g_l"
    ROWLABEL "g_s"
    ROWLABEL "g_rot"
    EDITABLE
    ONINSERTCHAR "caca"
    END
    WID_TABLE_3 TABLE 157 4 0 0
    N_ROWS = 4
    N_COLS = 1
    NUMCOLLABELS = 1
    COLLABEL "Value"
    NUMROWLABELS = 4
    ROWLABEL "Lambda"
    ROWLABEL "S"
    ROWLABEL "N"
    ROWLABEL "J"
    END
    WID_SLIDER_1 SLIDER 270 119 28 26
    VALUE = 1
    MAXIMUM = 2
    VERTICAL
    END
    WID_TEXT_1 TEXT 97 216 62 18
    WIDTH = 20
    HEIGHT = 1
    END
    WID_LABEL_4 LABEL 230 125 0 0
    VALUE "Delta J"
    ALIGNLEFT
    END
    WID_LABEL_5 LABEL 9 219 0 0
    VALUE "Hamiltonian size"
    ALIGNLEFT
    END
  END
  WID_LABEL_6 LABEL 442 9 0 0
  VALUE "Lower level quantum numbers"
  ALIGNLEFT
  END
  WID_LABEL_7 LABEL 305 12 0 0
  VALUE "Lower level constants"
  ALIGNLEFT
  END
  WID_BASE_3 BASE 649 26 145 196
  XPAD = 3
  YPAD = 3
  SPACE = 3
  BEGIN
    WID_BASE_4 BASE 16 21 0 0
    COLUMNS = 1
    EXCLUSIVE
    BEGIN
      WID_BUTTON_0 PUSHBUTTON -1 -1 0 0
      VALUE "Magnetic field"
      ALIGNLEFT
      END
      WID_BUTTON_3 PUSHBUTTON -1 -1 0 0
      VALUE "No magnetic field"
      ALIGNLEFT
      END
    END
    WID_TEXT_2 TEXT 21 110 94 19
    EDITABLE
    WIDTH = 20
    HEIGHT = 1
    END
    WID_LABEL_8 LABEL 21 93 0 0
    VALUE "Magnetic field"
    ALIGNLEFT
    END
    WID_LABEL_9 LABEL 122 112 0 0
    VALUE "G"
    ALIGNLEFT
    END
    WID_BUTTON_2 PUSHBUTTON 15 148 109 29
    VALUE "Zeeman Pattern"
    ALIGNCENTER
    ONACTIVATE "caquita"
    END
  END
  WID_BASE_6 BASE 8 295 296 212
  XPAD = 3
  YPAD = 3
  SPACE = 3
  BEGIN
    WID_TABLE_4 TABLE 0 18 0 0
    N_ROWS = 2
    N_COLS = 3
    NUMCOLLABELS = 4
    COLLABEL "Energy"
    COLLABEL "Splitting"
    COLLABEL "g_lande"
    COLLABEL ""
    END
    WID_LABEL_10 LABEL 4 2 0 0
    VALUE "Upper level eigenvalues"
    ALIGNLEFT
    END
  END
  WID_BASE_7 BASE 328 296 296 212
  XPAD = 3
  YPAD = 3
  SPACE = 3
  BEGIN
    WID_TABLE_5 TABLE 0 19 0 0
    N_ROWS = 2
    N_COLS = 3
    NUMCOLLABELS = 4
    COLLABEL "Energy"
    COLLABEL "Splitting"
    COLLABEL "g_lande"
    COLLABEL ""
    END
    WID_LABEL_11 LABEL 4 2 0 0
    VALUE "Lower level eigenvalues"
    ALIGNLEFT
    END
  END
END
