*&---------------------------------------------------------------------*
*& Report Z_SCF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SCF.

Data: pattern_table type standard table of string with empty key,
      pattern       type c length 75.

SELECTION-SCREEN BEGIN OF BLOCK b1k2 WITH FRAME TITLE text-001.
  PARAMETERS: p_meth as CHECKBOX DEFAULT 'X',
              p_enho as checkbox,
              p_dtp  as checkbox,
              p_trfn as checkbox.
SELECTION-SCREEN END OF BLOCK b1k2.

SELECTION-SCREEN BEGIN OF BLOCK b1k3 WITH FRAME TITLE text-002.
  SELECT-OPTIONS: s_patter FOR pattern NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1k3.

Loop at s_patter ASSIGNING FIELD-SYMBOL(<ls_pat>).
  Append '*' && <ls_pat>-low && '*' to pattern_table.
ENDLOOP.

zcl_scf=>run( EXPORTING iv_methods = p_meth
                      iv_dtp     = p_dtp
                      iv_trfn    = p_trfn
                      iv_enho    = p_enho
                      it_pattern = pattern_table[] ).
