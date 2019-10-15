*&---------------------------------------------------------------------*
*& Report Z_SCF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SCF.

Data: lt_pat type table of string,
      lv_pat type c length 75.

SELECTION-SCREEN BEGIN OF BLOCK b1k2 WITH FRAME TITLE text-001.
  PARAMETERS: p_meth as CHECKBOX DEFAULT 'X',
              p_enho as checkbox,
              p_dtp  as checkbox,
              p_trfn as checkbox.
  SELECT-OPTIONS: s_patter FOR lv_pat.
SELECTION-SCREEN END OF BLOCK b1k2.

Loop at s_patter ASSIGNING FIELD-SYMBOL(<ls_pat>).
  Append '*' && <ls_pat>-low && '*' to lt_pat.
ENDLOOP.

z_scf=>run( EXPORTING i_methods = p_meth
                      i_dtp     = p_dtp
                      i_trfn    = p_trfn
                      i_enho    = p_enho
                      i_pattern = lt_pat ).
