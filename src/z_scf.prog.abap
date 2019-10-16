*&---------------------------------------------------------------------*
*& Report Z_SCF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SCF.

Data: pattern_table type table of string,
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

z_scf=>run( EXPORTING i_methods = p_meth
                      i_dtp     = p_dtp
                      i_trfn    = p_trfn
                      i_enho    = p_enho
                      i_pattern = pattern_table ).
