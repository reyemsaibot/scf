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

REPORT zap_scf_standalone.

CLASS zcl_scf DEFINITION DEFERRED.
"#autoformat
"! <p class="shorttext synchronized" lang="en">Source Code Finder</p>
CLASS zcl_scf DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en"></p>
    ty_t_string TYPE STANDARD TABLE OF string WITH EMPTY KEY .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter i_methods | <p class="shorttext synchronized" lang="en">Search Methods</p>
    "! @parameter i_dtp     | <p class="shorttext synchronized" lang="en">Search DTPs</p>
    "! @parameter i_trfn    | <p class="shorttext synchronized" lang="en">Search Transformations</p>
    "! @parameter i_enho    | <p class="shorttext synchronized" lang="en">Search Enhancements</p>
    CLASS-METHODS run
    IMPORTING
      !iv_methods TYPE boolean
      !iv_dtp TYPE boolean
      !iv_trfn TYPE boolean
      !it_pattern TYPE ty_t_string
      !iv_enho TYPE boolean.

    CLASS-METHODS consider_methods
    IMPORTING
      !iv_methods TYPE rs_bool .
    CLASS-METHODS check_in_methods
    RETURNING
      VALUE(rv_result) TYPE rs_bool .
    CLASS-METHODS consider_amdp_routines
    IMPORTING
      !iv_amdp TYPE rs_bool .
    CLASS-METHODS check_amdp_routines
    RETURNING
      VALUE(rv_result) TYPE rs_bool .
    CLASS-METHODS set_search_pattern
    IMPORTING
      !iv_search_pattern TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en"></p>
    BEGIN OF ty_code,
        lv_class   TYPE string,
        lv_method  TYPE string,
        lv_line    TYPE i,
        lv_code    TYPE string,
        lv_pattern TYPE string,
      END OF ty_code .
    TYPES:
      "! <p class="shorttext synchronized" lang="en"></p>
    ty_t_code TYPE STANDARD TABLE OF ty_code WITH EMPTY KEY .

    TYPES:   "! <p class="shorttext synchronized" lang="en">Table of Data Transfer Processes</p>
    ty_t_dtps TYPE STANDARD TABLE OF rsbkdtpnm WITH DEFAULT KEY .
    TYPES:
    ty_t_objectlist TYPE STANDARD TABLE OF sobj_name WITH EMPTY KEY .

    CONSTANTS default_customer_space TYPE sobj_name VALUE 'Z%' ##NO_TEXT.
    CLASS-DATA gv_methods TYPE rs_bool .
    CLASS-DATA gv_amdp TYPE rs_bool .
    CLASS-DATA gv_pattern TYPE string .

    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter IT_REPORT | <p class="shorttext synchronized" lang="en">ABAP Report</p>
    "! @parameter I_METHOD  | <p class="shorttext synchronized" lang="en">Method</p>
    "! @parameter I_PATTERN | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE   | <p class="shorttext synchronized" lang="en">Return found Code Lines</p>
    CLASS-METHODS get_code
    IMPORTING
      !it_report TYPE ty_t_string
      !is_method TYPE seop_method_w_include
    RETURNING
      VALUE(rt_code) TYPE ty_t_code .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter IT_TABLE       | <p class="shorttext synchronized" lang="en">Output</p>
    "! @parameter IT_DESCRIPTION | <p class="shorttext synchronized" lang="en">Description of Columns</p>
    CLASS-METHODS get_output
    IMPORTING
      !it_table TYPE STANDARD TABLE
      !it_description TYPE slis_t_fieldcat_alv .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter I_pattern | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter rt_code   | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS trfn
    IMPORTING
      !it_pattern TYPE ty_t_string
    RETURNING
      VALUE(rt_code) TYPE ty_t_code .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter i_value   | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter I_pattern | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE   | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS get_code_from_methods
    IMPORTING
      !iv_type TYPE trobjtype
      !iv_object_name TYPE sobj_name
    RETURNING
      VALUE(rt_code) TYPE ty_t_code .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter IT_METHODS | <p class="shorttext synchronized" lang="en">Table with all ABAP source code</p>
    "! @parameter I_PATTERN  | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE    | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS read_report
    IMPORTING
      !it_methods TYPE seop_methods_w_include
    RETURNING
      VALUE(rt_code) TYPE ty_t_code .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter I_PATTERN  | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE    | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS dtp
    IMPORTING
      !it_pattern TYPE ty_t_string
    RETURNING
      VALUE(rt_code) TYPE ty_t_code .
    CLASS-METHODS _get_all_dpts
    RETURNING
      VALUE(rt_dtps) TYPE ty_t_dtps .
    CLASS-METHODS _get_objects_from_tadir
    IMPORTING
      !iv_type TYPE trobjtype
      !iv_object_name TYPE sobj_name
    RETURNING
      VALUE(rt_objectlist) TYPE ty_t_objectlist .
    CLASS-METHODS get_pattern
    RETURNING
      VALUE(rv_result) TYPE string .

ENDCLASS.
CLASS zcl_scf IMPLEMENTATION.
  METHOD check_amdp_routines.
    rv_result = gv_amdp.
  ENDMETHOD.
  METHOD check_in_methods.
    rv_result = gv_methods.
  ENDMETHOD.
  METHOD consider_amdp_routines.
    gv_amdp = iv_amdp.
  ENDMETHOD.
  METHOD consider_methods.
    gv_methods = iv_methods.
  ENDMETHOD.
  METHOD dtp.
    DATA: lt_code TYPE ty_t_code.

    LOOP AT _get_all_dpts( ) REFERENCE INTO DATA(ls_dtp).
      TRY.
          lt_code = VALUE #( BASE lt_code FOR ls_pattern IN it_pattern
          FOR ls_dtprule IN cl_rsbk_dtp=>factory( i_dtp = ls_dtp->* )->get_obj_ref_filter( )->n_t_dtprule
          WHERE ( line CP ls_pattern )
                                               ( lv_method  = ls_dtp->*
                                                 lv_class   = ls_dtprule-field
                                                 lv_line    = ls_dtprule-line_no
                                                 lv_code    = ls_dtprule-line
                                                 lv_pattern = ls_pattern ) ).
        CATCH cx_rs_access_error.
      ENDTRY.
    ENDLOOP.
    rt_code = lt_code.
  ENDMETHOD.
  METHOD get_code.
    DATA(lv_search_pattern) = get_pattern( ).
    rt_code = VALUE #( BASE rt_code FOR <ls_report> IN it_report WHERE ( table_line CP lv_search_pattern )
                            ( lv_method  = is_method-cpdkey-cpdname
                              lv_class   = is_method-cpdkey-clsname
                              lv_line    = sy-tabix
                              lv_code    = <ls_report>
                              lv_pattern = lv_search_pattern ) ).
  ENDMETHOD.
  METHOD get_code_from_methods.
    DATA: lt_methods TYPE seop_methods_w_include.

    DATA(lt_objects) = _get_objects_from_tadir( iv_type        = iv_type
                                                iv_object_name = iv_object_name ).

    LOOP AT lt_objects REFERENCE INTO DATA(ls_objects).
      "Get all Methods
      cl_oo_classname_service=>get_all_method_includes( EXPORTING clsname = CONV #( ls_objects->* )
                                                        RECEIVING result = DATA(lt_seop_methods_w_include)
                                                        EXCEPTIONS class_not_existing = 1 ).

      CHECK sy-subrc = 0.
      APPEND LINES OF lt_seop_methods_w_include TO lt_methods.
      CLEAR lt_seop_methods_w_include.
    ENDLOOP.
    IF lt_methods[] IS NOT INITIAL.
      rt_code = read_report( lt_methods ).
    ENDIF.
  ENDMETHOD.
  METHOD get_output.

    DATA: lo_dref TYPE REF TO data.
    DATA: lv_layout TYPE slis_layout_alv.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

    lv_layout-colwidth_optimize  = rs_c_true.

    CREATE DATA lo_dref LIKE it_table.
    ASSIGN lo_dref->* TO <fs_table>.

    <fs_table> = it_table.

    TRY.
        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            is_layout   = lv_layout
            it_fieldcat = it_description
          TABLES
            t_outtab    = <fs_table>.

      CATCH cx_salv_not_found.
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.
  METHOD get_pattern.
    rv_result = gv_pattern.
  ENDMETHOD.
  METHOD read_report.
    DATA: lt_report TYPE ty_t_string.
    DATA:     lt_code TYPE TABLE OF ty_code.

    LOOP AT it_methods ASSIGNING FIELD-SYMBOL(<ls_methods>).
      READ REPORT <ls_methods>-incname INTO lt_report.
      CHECK sy-subrc = 0.
      APPEND LINES OF get_code( it_report = lt_report
                                is_method = <ls_methods> ) TO lt_code.

      CLEAR: lt_report.
    ENDLOOP.
    rt_code = lt_code.
  ENDMETHOD.
  METHOD run.
**********************************************************************
* Author: T.Meyer, https://www.reyemsaibot.com, 26.04.2019
**********************************************************************
*
* Search for specific pattern in several ABAP development objects
*
**********************************************************************
* Change log
**********************************************************************
* 26.04.19 TM initial version
* 23.09.19 TM Change to own class
* 08.10.21 TM Changes to ABAPLint
**********************************************************************
*Kate: "Sagen sie, bauen alle Marines Boote?"
*Tony: "Nur die, die mehrfach verheiratet waren."
*Kate: "Wieso das?"
*Tony: "Die anderen koennen sich welche kaufen."

    DATA: lt_code     TYPE ty_t_code,
          lt_fieldcat TYPE slis_t_fieldcat_alv,
          ls_fieldcat TYPE slis_fieldcat_alv.

    IF check_in_methods( ).
      APPEND LINES OF get_code_from_methods( iv_type        = 'CLAS'
                                             iv_object_name = default_customer_space ) TO lt_code.
    ENDIF.

    IF check_amdp_routines( ).
      APPEND LINES OF get_code_from_methods( iv_type        = 'CLAS'
                                             iv_object_name = '/BIC/%' ) TO lt_code.
    ENDIF.

    IF iv_enho = rs_c_true.
      APPEND LINES OF get_code_from_methods( iv_type        = 'ENHO'
                                             iv_object_name = default_customer_space ) TO lt_code.
    ENDIF.

    IF iv_dtp = rs_c_true.
      lt_code = dtp( it_pattern ).
    ENDIF.

    IF iv_trfn = rs_c_true.
      lt_code = trfn( it_pattern ).
    ENDIF.

    IF lt_code[] IS NOT INITIAL.

      ls_fieldcat-fieldname = 'LV_PATTERN'.
      ls_fieldcat-seltext_m = 'Pattern'.
      APPEND ls_fieldcat TO lt_fieldcat.

      ls_fieldcat-fieldname = 'LV_CLASS'.
      ls_fieldcat-seltext_m = 'Class/InfoObject/Source'.
      APPEND ls_fieldcat TO lt_fieldcat.

      ls_fieldcat-fieldname = 'LV_METHOD'.
      ls_fieldcat-seltext_m = 'Method/TRFN/DTP'.
      APPEND ls_fieldcat TO lt_fieldcat.

      ls_fieldcat-fieldname = 'LV_LINE'.
      ls_fieldcat-seltext_m = 'Line'.
      APPEND ls_fieldcat TO lt_fieldcat.

      ls_fieldcat-fieldname = 'LV_CODE'.
      ls_fieldcat-seltext_m = 'Code'.
      APPEND ls_fieldcat TO lt_fieldcat.

      get_output( it_table       = lt_code
                  it_description = lt_fieldcat ).

    ELSE.
      MESSAGE 'No entry found' TYPE 'I'.
    ENDIF.

  ENDMETHOD.
  METHOD set_search_pattern.
    gv_pattern = iv_search_pattern.
  ENDMETHOD.
  METHOD trfn.

    TYPES: BEGIN OF ty_trans_lookup_finder,
             targetname TYPE rstran-targetname,
             sourcename TYPE rstran-sourcename,
             tranid     TYPE rstran-tranid,
             routine    TYPE rstran-startroutine,
             line_no    TYPE rsaabap-line_no,
             line       TYPE rsaabap-line,
           END OF ty_trans_lookup_finder.

    DATA: lt_lookup_finder       TYPE STANDARD TABLE OF ty_trans_lookup_finder,
          lt_trans_lookup_finder TYPE STANDARD TABLE OF ty_trans_lookup_finder,
          lt_code                TYPE ty_t_code,
          ls_code                TYPE ty_code.

    SELECT DISTINCT
            infocube,
            isource,
            b~updid,
            routine,
            line_no,
            line
            INTO TABLE @lt_lookup_finder FROM
            rsupdinfo AS a  INNER JOIN rsupdrout AS b ON a~updid = b~updid
                            INNER JOIN rsaabap AS c ON c~codeid = b~codeid
                            WHERE
                            a~objvers = 'A' AND b~objvers = 'A' AND
                            c~objvers = 'A' AND a~objvers = 'A'.
    SELECT DISTINCT
            targetname,
            sourcename,
            a~tranid,
            c~codeid,
            line_no,
            line
            INTO TABLE @lt_trans_lookup_finder FROM
            rstran AS a INNER JOIN rstransteprout AS b
                            ON a~tranid = b~tranid
                        INNER JOIN rsaabap AS c ON b~codeid = c~codeid
                            WHERE
                            a~objvers = 'A' AND b~objvers = 'A' AND
                            c~objvers = 'A'.

    APPEND LINES OF lt_trans_lookup_finder TO lt_lookup_finder.

***Selections for Transformations(start routine)*********************
    SELECT DISTINCT
           targetname,
           sourcename,
           tranid,
           startroutine,
           line_no,
           line
           INTO TABLE @lt_trans_lookup_finder FROM
          rstran AS a INNER JOIN rsaabap AS b ON a~startroutine = b~codeid
                           WHERE
                           a~objvers = 'A' AND b~objvers = 'A'.

    APPEND LINES OF lt_trans_lookup_finder TO lt_lookup_finder.

***Selections for Transformations(End routine)***********************
    SELECT DISTINCT
           targetname,
           sourcename,
           tranid,
           endroutine,
           line_no,
           line
           INTO TABLE @lt_trans_lookup_finder FROM
           rstran AS a  INNER JOIN rsaabap AS b ON a~endroutine = b~codeid
                           WHERE
                           a~objvers = 'A' AND b~objvers = 'A'.

    APPEND LINES OF lt_trans_lookup_finder TO lt_lookup_finder.

***Selections for Transformations(Expert routine)********************
    SELECT DISTINCT
           targetname,
           sourcename,
           tranid,
           expert,
           line_no,
           line
           INTO TABLE @lt_trans_lookup_finder FROM
           rstran AS a  INNER JOIN rsaabap AS b ON a~expert = b~codeid
                           WHERE
                           a~objvers = 'A' AND b~objvers = 'A'.

    APPEND LINES OF lt_trans_lookup_finder TO lt_lookup_finder.

***Extracting records where lookup code is written*******************
    SORT lt_lookup_finder BY targetname sourcename tranid routine line_no.

    LOOP AT lt_lookup_finder REFERENCE INTO DATA(ls_lookup_finder).
      TRANSLATE ls_lookup_finder->line TO UPPER CASE.
      LOOP AT it_pattern ASSIGNING FIELD-SYMBOL(<ls_pattern>).
        IF ( ls_lookup_finder->line CP <ls_pattern> ).
          ls_code-lv_class    = ls_lookup_finder->sourcename.
          ls_code-lv_code     = ls_lookup_finder->line.
          ls_code-lv_line     = ls_lookup_finder->line_no.
          ls_code-lv_method   = ls_lookup_finder->tranid.
          ls_code-lv_pattern  = <ls_pattern>.
          APPEND ls_code TO lt_code.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    rt_code = lt_code.
  ENDMETHOD.
  METHOD _get_all_dpts.
    SELECT DISTINCT dtp
     FROM rsbkdtp
     INTO TABLE @rt_dtps.

    DELETE ADJACENT DUPLICATES FROM rt_dtps.
  ENDMETHOD.
  METHOD _get_objects_from_tadir.
    SELECT obj_name
     FROM tadir
     INTO TABLE @rt_objectlist
    WHERE pgmid = 'R3TR'
      AND object = @iv_type
      AND obj_name LIKE @iv_object_name.
  ENDMETHOD.
ENDCLASS.
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

****************************************************
INTERFACE lif_abapmerge_marker.
* abapmerge 0.14.3 - 2021-11-22T08:55:52.981Z
ENDINTERFACE.
****************************************************