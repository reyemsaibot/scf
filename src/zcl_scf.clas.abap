"#autoformat
"! <p class="shorttext synchronized" lang="en">Source Code Finder</p>
CLASS zcl_scf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Table of strings</p>
      ty_t_string TYPE STANDARD TABLE OF string WITH EMPTY KEY.

    "! <p class="shorttext synchronized" lang="en">Search for Source Code in TRFN, DTPs, Methods</p>
    "!
    "! @parameter i_methods | <p class="shorttext synchronized" lang="en">Search Methods</p>
    "! @parameter i_dtp     | <p class="shorttext synchronized" lang="en">Search DTPs</p>
    "! @parameter i_trfn    | <p class="shorttext synchronized" lang="en">Search Transformations</p>
    "! @parameter i_enho    | <p class="shorttext synchronized" lang="en">Search Enhancements</p>
    CLASS-METHODS run
      IMPORTING
        !iv_methods TYPE boolean
        !iv_dtp     TYPE boolean
        !iv_trfn    TYPE boolean
        !it_pattern TYPE ty_t_string
        !iv_enho    TYPE boolean.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure of code line output</p>
      BEGIN OF ty_code,
        lv_class   TYPE string,
        lv_method  TYPE string,
        lv_line    TYPE i,
        lv_code    TYPE string,
        lv_pattern TYPE c LENGTH 75,
      END OF ty_code.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Table of code line output</p>
      ty_t_code TYPE STANDARD TABLE OF ty_code WITH EMPTY KEY.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">ABAP reports</p>
      BEGIN OF ty_report,
        row TYPE string,
      END OF ty_report.

    TYPES: "! <p class="shorttext synchronized" lang="en">Table of ABAP reports</p>
      ty_t_report TYPE STANDARD TABLE OF ty_report.

    "! <p class="shorttext synchronized" lang="en">Get ABAP Code</p>
    "!
    "! @parameter IT_REPORT | <p class="shorttext synchronized" lang="en">ABAP Report</p>
    "! @parameter I_METHOD  | <p class="shorttext synchronized" lang="en">Method</p>
    "! @parameter I_PATTERN | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE   | <p class="shorttext synchronized" lang="en">Return found Code Lines</p>
    CLASS-METHODS get_code
      IMPORTING
        !it_report     TYPE ty_t_report
        !is_method     TYPE seocpdkey
        !it_pattern    TYPE ty_t_string
      RETURNING
        VALUE(rt_code) TYPE ty_t_code.

    "! <p class="shorttext synchronized" lang="en">Get Output ALV GRID</p>
    "!
    "! @parameter IT_TABLE       | <p class="shorttext synchronized" lang="en">Output</p>
    "! @parameter IT_DESCRIPTION | <p class="shorttext synchronized" lang="en">Description of Columns</p>
    CLASS-METHODS get_output
      IMPORTING
        !it_table       TYPE STANDARD TABLE
        !it_description TYPE slis_t_fieldcat_alv.

    "! <p class="shorttext synchronized" lang="en">Search Transformations</p>
    "!
    "! @parameter I_pattern | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter rt_code   | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS trfn
      IMPORTING
        !it_pattern    TYPE ty_t_string
      RETURNING
        VALUE(rt_code) TYPE ty_t_code.

    "! <p class="shorttext synchronized" lang="en">Get Method Sourcecode</p>
    "!
    "! @parameter i_value   | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter I_pattern | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE   | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS methods
      IMPORTING
        !iv_value      TYPE string
        !it_pattern    TYPE ty_t_string
      RETURNING
        VALUE(rt_code) TYPE ty_t_code.

    "! <p class="shorttext synchronized" lang="en">Read ABAP Report</p>
    "!
    "! @parameter IT_METHODS | <p class="shorttext synchronized" lang="en">Table with all ABAP source code</p>
    "! @parameter I_PATTERN  | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE    | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS read_report
      IMPORTING
        !it_methods    TYPE STANDARD TABLE
        !it_pattern    TYPE ty_t_string
      RETURNING
        VALUE(rt_code) TYPE ty_t_code.

    "! <p class="shorttext synchronized" lang="en">Get DTP Sourcecode</p>
    "!
    "! @parameter I_PATTERN  | <p class="shorttext synchronized" lang="en">Search Pattern</p>
    "! @parameter RT_CODE    | <p class="shorttext synchronized" lang="en">Return founded code lines</p>
    CLASS-METHODS dtp
      IMPORTING
        !it_pattern    TYPE ty_t_string
      RETURNING
        VALUE(rt_code) TYPE ty_t_code.

ENDCLASS.

CLASS zcl_scf IMPLEMENTATION.

  METHOD dtp.

    DATA: lt_dtprule       TYPE mch_t_sourcecode,
          ls_dtprule       TYPE REF TO mch_s_sourcecode,
          lo_dtp           TYPE REF TO cl_rsbk_dtp,
          lo_filter        TYPE REF TO cl_rsbc_filter,
          ls_code          TYPE ty_code,
          lt_code          TYPE ty_t_code.

    SELECT DISTINCT dtp
      FROM rsbkdtp
      INTO TABLE @DATA(lt_dtps)
      WHERE objvers = @rs_c_objvers-active.

    LOOP AT lt_dtps ASSIGNING FIELD-SYMBOL(<ls_dtp>).
      "Get DTP object
      lo_dtp = cl_rsbk_dtp=>factory( i_dtp = <ls_dtp>-dtp ).
      "Get Filter
      TRY.
          lo_filter = lo_dtp->get_obj_ref_filter( ).
          "Get source code
          APPEND LINES OF lo_filter->n_t_dtprule TO lt_dtprule.
          LOOP AT it_pattern ASSIGNING FIELD-SYMBOL(<ls_pattern>).
            LOOP AT lt_dtprule REFERENCE INTO ls_dtprule WHERE line CP <ls_pattern>.
              ls_code-lv_method  = <ls_dtp>.
              ls_code-lv_class   = ls_dtprule->field.
              ls_code-lv_line    = ls_dtprule->line_no.
              ls_code-lv_code    = ls_dtprule->line.
              ls_code-lv_pattern = <ls_pattern>.
              APPEND ls_code TO lt_code.
            ENDLOOP.
          ENDLOOP.
          CLEAR lt_dtprule.
        CATCH cx_rs_access_error.
      ENDTRY.
    ENDLOOP.
    rt_code = lt_code.
  ENDMETHOD.


  METHOD get_code.
    DATA: ls_code TYPE ty_code,
          lt_code TYPE TABLE OF ty_code.

    LOOP AT it_pattern ASSIGNING FIELD-SYMBOL(<ls_pattern>).
      LOOP AT it_report ASSIGNING FIELD-SYMBOL(<ls_report>) WHERE row CP <ls_pattern>.
        ls_code-lv_method  = is_method-cpdname.
        ls_code-lv_class   = is_method-clsname.
        ls_code-lv_line    = sy-tabix.
        ls_code-lv_code    = <ls_report>-row.
        ls_code-lv_pattern = <ls_pattern>.
        APPEND ls_code TO lt_code.
      ENDLOOP.
    ENDLOOP.
    rt_code = lt_code.
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


  METHOD methods.
    DATA: lt_methods TYPE ty_t_string,
          lv_methods TYPE string,
          ls_ob      TYPE seoclsname,
          lt_obj     TYPE STANDARD TABLE OF sobj_name,
          lv_i       TYPE i VALUE 1.

    SELECT obj_name INTO TABLE @lt_obj FROM tadir WHERE pgmid  = 'R3TR'
                                                  AND  object = @iv_value
                                                  AND  obj_name LIKE 'Z%'.

    LOOP AT lt_obj INTO ls_ob.
      "Get all Methods
      cl_oo_classname_service=>get_all_method_includes( EXPORTING clsname = ls_ob
                                                        RECEIVING result  = DATA(lt_result)
                                                        EXCEPTIONS class_not_existing = 1 ).
      CHECK sy-subrc = 0.
      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_result>).
        SPLIT <ls_result> AT ' ' INTO TABLE DATA(lt_string).
        LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<ls_string>).
          IF <ls_string> = ''.
            "Skip empty line
          ELSEIF lv_i = 3.
            "Save method for later use
            lv_methods = <ls_string>.
          ELSE.
            lv_i = lv_i + 1.
          ENDIF.
        ENDLOOP.
        APPEND lv_methods TO lt_methods.
        lv_i = 1.
      ENDLOOP.
    ENDLOOP.

    IF lt_methods[] IS NOT INITIAL.
      rt_code = read_report( it_methods = lt_methods
                             it_pattern = it_pattern ).
    ENDIF.
  ENDMETHOD.


  METHOD read_report.
    DATA: lv_report     TYPE c LENGTH 60,
          lt_string     TYPE TABLE OF string,
          lt_report     TYPE ty_t_report,
          ls_report     TYPE ty_report,
          lv_incname    TYPE program,
          lt_code_final TYPE TABLE OF ty_code.

    LOOP AT it_methods ASSIGNING FIELD-SYMBOL(<ls_methods>).

      CONCATENATE '' <ls_methods> '' INTO lv_report.

      READ REPORT lv_report INTO lt_string.
      LOOP AT lt_string ASSIGNING FIELD-SYMBOL(<ls_string>).
        ls_report-row = <ls_string>.
        APPEND ls_report TO lt_report.
      ENDLOOP.
      lv_incname = lv_report.

      cl_oo_classname_service=>get_method_by_include(
        EXPORTING
          incname             = lv_incname
          with_enhancements   = rs_c_true
        RECEIVING
          mtdkey              = DATA(lv_method)
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2 ).

      CHECK sy-subrc = 0.

      DATA(lt_code) = get_code( it_report = lt_report
                                is_method  = lv_method
                                it_pattern = it_pattern ).

      APPEND LINES OF lt_code TO lt_code_final.
      CLEAR: lt_report.

    ENDLOOP.
    rt_code = lt_code_final.
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

    IF iv_methods = rs_c_true.
      lt_code = methods( iv_value   = 'CLAS'
                         it_pattern = it_pattern ).
    ENDIF.

    IF iv_enho = rs_c_true.
      lt_code = methods( iv_value   = 'ENHO'
                         it_pattern = it_pattern ).
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
      ls_fieldcat-seltext_m = 'Method/TRFN/TRFN'.
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
ENDCLASS.