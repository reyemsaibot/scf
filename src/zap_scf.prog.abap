**********************************************************************
* Author: T.Meyer, https://www.reyemsaibot.com, 26.04.2019
**********************************************************************
*
* Search for pattern in ABAP development objects
*
**********************************************************************
* Change log
**********************************************************************
* 26.04.19 TM initial version
* 15.10.21 TM Add AMDP Check
**********************************************************************

REPORT zap_scf.

DATA: pattern_table TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      pattern       TYPE c LENGTH 75.

SELECTION-SCREEN BEGIN OF BLOCK b1k2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_meth AS CHECKBOX DEFAULT 'X',
            p_enho AS CHECKBOX,
            p_dtp  AS CHECKBOX,
            p_amdp AS CHECKBOX,
            p_trfn AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1k2.

SELECTION-SCREEN BEGIN OF BLOCK b1k3 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: s_patter FOR pattern NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1k3.

zcl_scf=>set_search_pattern( CONV #( s_patter-low ) ).

zcl_scf=>consider_methods( p_meth ).
zcl_scf=>consider_amdp_routines( p_amdp ).



zcl_scf=>run( iv_methods = p_meth
              iv_dtp     = p_dtp
              iv_trfn    = p_trfn
              iv_enho    = p_enho
              it_pattern = pattern_table[] ).