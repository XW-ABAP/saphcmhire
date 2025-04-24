FUNCTION zXXX_hr_pafm_datatoprelp.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_INFTY) TYPE  INFTY
*"     VALUE(IV_DATA) TYPE  STRING
*"     VALUE(IV_MOLGA) TYPE  VIEKN DEFAULT '28'
*"     VALUE(IV_VERSION) TYPE  ZXXX_HR_PA_E_ZVERSION OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_PRELP) TYPE  PRELP_TAB
*"     REFERENCE(ET_MESSAGE) TYPE  ZXXX_HR_PA_TAB_MESSAGE
*"  TABLES
*"      IT_TPINT STRUCTURE  ZXXX_HR_PA_TPINT OPTIONAL
*"--------------------------------------------------------------------
  DATA:lv_message    TYPE string,
       lv_messageall TYPE string,
       ls_tpintnew   LIKE LINE OF it_tpint,
       lt_tpintnew   LIKE TABLE OF ls_tpintnew,
       ls_tpintsap   LIKE LINE OF it_tpint,
       lt_tpintsap   LIKE TABLE OF ls_tpintsap,
       lr_descr      TYPE REF TO cl_abap_structdescr,
       ls_tpintfind  LIKE LINE OF   it_tpint,
       lt_tpintfind  LIKE TABLE OF  ls_tpintfind,
       lv_tabname    TYPE tabname,
       lt_prelp      TYPE  prelp_tab,
       lt_message    TYPE zXXX_hr_pa_tab_message,
       lt_comp       TYPE cl_abap_structdescr=>component_table,
       ls_comp       LIKE LINE OF lt_comp,
       lr_line       TYPE REF TO data,
       lr_itab       TYPE REF TO data,
       lr_sapline    TYPE REF TO data,
       lr_sapitab    TYPE REF TO data,
       lr_struc      TYPE REF TO cl_abap_structdescr,
       lr_table      TYPE REF TO cl_abap_tabledescr.
  FIELD-SYMBOLS:<lt_itab>    TYPE STANDARD TABLE,
                <ls_itab>    TYPE any,
                <lt_sapitab> TYPE STANDARD TABLE,
                <ls_sapitab> TYPE any.

  CLEAR:lt_tpintnew.
  APPEND LINES OF it_tpint TO lt_tpintnew. " Assignment processing
  DELETE lt_tpintnew WHERE infty <> iv_infty.
  SORT lt_tpintnew BY znsapfield.          " Sort processing
  "Temporary dynamic inner tables are used to convert the other system's fields into our system fields
  DELETE ADJACENT DUPLICATES FROM lt_tpintnew COMPARING znsapfield.

*  CLEAR:lt_tpintsap.
*  APPEND LINES OF it_tpint TO lt_tpintsap.  " Assignment processing
*  DELETE lt_tpintsap WHERE infty <> iv_infty.
*  SORT lt_tpintsap BY fieldname.            " Sort processing
*  "Temporary dynamic inner tables are used to convert the other system's fields into our system fields
*  DELETE ADJACENT DUPLICATES FROM lt_tpintsap COMPARING fieldname. "

  CLEAR:lt_tpintfind.
  APPEND LINES OF it_tpint TO lt_tpintfind.
  DELETE lt_tpintfind WHERE infty <> iv_infty.
  SORT lt_tpintfind BY fieldname znsapvalue.

  " First turn the data into a fake dynamic inner table
*  LOOP AT lt_tpintnew ASSIGNING FIELD-SYMBOL(<ls_tpintnew>) WHERE infty = iv_infty.
*    REPLACE ALL OCCURRENCES OF <ls_tpintnew>-znsapfield IN iv_data WITH <ls_tpintnew>-fieldname.
*  ENDLOOP.


  CLEAR:lv_messageall.

  CLEAR:lt_comp.
*       Generate a dynamic in-table of the fields of our SAP
*  LOOP AT lt_tpintsap ASSIGNING FIELD-SYMBOL(<ls_tpintsap>) WHERE infty = iv_infty.
  LOOP AT lt_tpintnew ASSIGNING FIELD-SYMBOL(<ls_tpintnew>) WHERE infty = iv_infty.
    TRANSLATE <ls_tpintnew>-znsapfield TO UPPER CASE.
    IF <ls_tpintnew>-znsapfield IS NOT INITIAL.
      TRY.
          ls_comp-type ?= cl_abap_elemdescr=>get_string( ). "Gets a field of type string
        CATCH cx_parameter_invalid_range INTO DATA(lr_cx). " Parameter with invalid value range.
          lv_message = lr_cx->get_text( ).

          lv_messageall = lv_messageall && lv_message.
*              MESSAGE lv_message TYPE 'E'.
      ENDTRY.
      ls_comp-name = <ls_tpintnew>-znsapfield.               "Dynamic in-table fields are given names
      APPEND ls_comp TO lt_comp.
      CLEAR ls_comp.
    ENDIF.
  ENDLOOP.


  IF lt_comp[] IS INITIAL.
    lv_message = TEXT-001.  "Determine whether the structure is empty
    lv_messageall = lv_messageall && lv_message.
  ELSE.
    TRY. "Production dynamic internal table structure
        lr_struc = cl_abap_structdescr=>create( lt_comp ).
      CATCH cx_sy_struct_attributes INTO DATA(lr_cx_sturct).
        lv_message = lr_cx_sturct->get_text( ).
*            MESSAGE e000(zXXX_hr_py_pay0001) WITH lv_message.
        lv_messageall = lv_messageall && lv_message.
    ENDTRY.

*Generate table type dynamically
    TRY.
        CALL METHOD cl_abap_tabledescr=>create
          EXPORTING
            p_line_type  = lr_struc
            p_table_kind = 'S'
            p_key_kind   = 'D'
          RECEIVING
            p_result     = lr_table.
      CATCH cx_sy_table_creation INTO DATA(lr_cx_table).
        lv_message = lr_cx_table->get_text( ).
*            MESSAGE e000(zXXX_hr_py_pay0001) WITH lv_message.
        lv_messageall = lv_messageall && lv_message.
    ENDTRY.

*Generate internal table via the dynamic table type
    CREATE DATA lr_itab TYPE HANDLE lr_table.
    ASSIGN lr_itab->* TO <lt_itab>.
*Generate work area
    CREATE DATA lr_line TYPE HANDLE lr_struc.
    ASSIGN lr_line->* TO <ls_itab>.
  ENDIF.

  CHECK <lt_itab> IS ASSIGNED.  "Check whether a dynamic inner table is specified

  CALL METHOD /ui2/cl_json=>deserialize  "The information type is now JSON transformation dimension, and the dynamic inner table is now present
    EXPORTING
      json = iv_data
    CHANGING
      data = <lt_itab>. "Convert data into a dimensional table

*  Create a new information type within the table
  SELECT SINGLE
    molga,
    infty,
    vinft,
    dname,
    repid,
    dynnr
    FROM t582v
    WHERE molga = @iv_molga
      AND infty = @iv_infty
  INTO @DATA(ls_t582v).
  IF sy-subrc = 0.
    SELECT
      vinft,
      seqnr,
      infty
      FROM t582w
      WHERE vinft = @ls_t582v-vinft
      INTO TABLE @DATA(lt_t582w).
    IF sy-subrc = 0.
      DELETE lt_t582w WHERE infty = iv_infty.
      CLEAR:lt_comp,
            ls_comp.

      DATA(lv_newtabname) = |P| && iv_infty.
      lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_newtabname ).
      DATA(lt_fields) = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                                p_including_substructres = abap_true ).
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        DATA(lv_fieldname) =  <ls_fields>-tabname && '-' && <ls_fields>-fieldname.
        TRY.
            ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( p_name = lv_fieldname ).
          CATCH cx_parameter_invalid_range INTO lr_cx. " Parameter with invalid value range.
            CLEAR:lv_message.
            lv_message = lr_cx->get_text( ).
            lv_messageall = lv_messageall && lv_message.
        ENDTRY.
        ls_comp-name = <ls_fields>-fieldname.               "Dynamic in-table fields are given names
        APPEND ls_comp TO lt_comp.
        CLEAR ls_comp.
      ENDLOOP.
      LOOP AT lt_t582w ASSIGNING FIELD-SYMBOL(<ls_t582w>).
        CLEAR:lv_newtabname.
        lv_newtabname = |PS| && <ls_t582w>-infty.
        lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_newtabname ).
        CLEAR:lt_fields.
        lt_fields = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                                p_including_substructres = abap_true ).
        LOOP AT lt_fields ASSIGNING <ls_fields>.
          CLEAR:ls_comp.
          READ TABLE lt_comp INTO ls_comp WITH KEY name = <ls_fields>-fieldname. "如果找到重复的就不要了
          IF sy-subrc <> 0.
            CLEAR:lv_fieldname.
            lv_fieldname =  <ls_fields>-tabname && '-' && <ls_fields>-fieldname.
            TRY.
                ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( p_name = lv_fieldname ).
              CATCH cx_parameter_invalid_range INTO lr_cx. " Parameter with invalid value range.
                CLEAR:lv_message.
                lv_message = lr_cx->get_text( ).
                lv_messageall = lv_messageall && lv_message.
            ENDTRY.
            ls_comp-name = <ls_fields>-fieldname.               "Dynamic in-table fields are given names
            APPEND ls_comp TO lt_comp.
            CLEAR ls_comp.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      IF lt_comp[] IS INITIAL.
        lv_message = TEXT-001.  "Determine whether the structure is empty
      ELSE.
        TRY. "Production dynamic internal table structure
            lr_struc = cl_abap_structdescr=>create( lt_comp ).
          CATCH cx_sy_struct_attributes INTO lr_cx_sturct.
            lv_message = lr_cx_sturct->get_text( ).
            lv_messageall = lv_messageall && lv_message.
*            MESSAGE e000(zXXX_hr_py_pay0001) WITH lv_message.
        ENDTRY.

*Generate table type dynamically
        TRY.
            CALL METHOD cl_abap_tabledescr=>create
              EXPORTING
                p_line_type  = lr_struc
                p_table_kind = 'S'
                p_key_kind   = 'D'
              RECEIVING
                p_result     = lr_table.
          CATCH cx_sy_table_creation INTO lr_cx_table.
            lv_message = lr_cx_table->get_text( ).
*            MESSAGE e000(zXXX_hr_py_pay0001) WITH lv_message.
            lv_messageall = lv_messageall && lv_message.
        ENDTRY.

*Generate internal table via the dynamic table type
        CREATE DATA lr_sapitab TYPE HANDLE lr_table.
        ASSIGN lr_sapitab->* TO <lt_sapitab>.

*Generate work area
        CREATE DATA lr_sapline TYPE HANDLE lr_struc.
        ASSIGN lr_sapline->* TO <ls_sapitab>.

      ENDIF.
    ENDIF.
  ELSE.
    CLEAR:lv_tabname.
    lv_tabname = |P| && iv_infty.
    CREATE DATA lr_sapitab TYPE TABLE OF (lv_tabname).
    ASSIGN lr_sapitab->* TO <lt_sapitab>.
    CREATE DATA lr_sapline LIKE LINE OF <lt_sapitab>.
    ASSIGN lr_sapline->* TO <ls_sapitab>.
  ENDIF.

  CHECK <lt_sapitab> IS ASSIGNED.

  LOOP AT <lt_itab> ASSIGNING <ls_itab>.
    APPEND INITIAL LINE TO <lt_sapitab> ASSIGNING <ls_sapitab>.
*    LOOP AT lt_tpintsap ASSIGNING FIELD-SYMBOL(<ls_tpintsap>).
    LOOP AT lt_tpintnew ASSIGNING <ls_tpintnew>.
*      ASSIGN COMPONENT <ls_tpintsap>-fieldname OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_source>).
      ASSIGN COMPONENT <ls_tpintnew>-znsapfield OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_source>).
      IF sy-subrc = 0.
*        IF <ls_tpintsap>-fieldname EQ 'PERNR'. "Even numbers are assigned to odd numbers
        IF <ls_tpintnew>-fieldname EQ 'PERNR'.
          SELECT
             infty,
            zversion,
            znumber,
            fnsource,
            sntarget
          FROM zXXX_hr_pa_move
            WHERE infty = @iv_infty
              AND zversion = @iv_version
              AND fnsource = @<ls_tpintnew>-fieldname
              AND znumber >= '5551'
              AND znumber <= '7000'
           INTO TABLE @DATA(lt_move).
          SORT lt_move BY znumber.
          DATA: lv_shu TYPE i.
          LOOP AT lt_move ASSIGNING FIELD-SYMBOL(<ls_move>) WHERE fnsource = <ls_tpintnew>-fieldname.
            lv_shu = lv_shu + 1.
            IF  lv_shu MOD 2 = 1.
              DATA(lv_soo) = <ls_move>-sntarget.
              CONDENSE lv_soo NO-GAPS.
            ENDIF.
            IF lv_shu MOD 2 = 0.
              DATA(lv_too) = <ls_move>-sntarget.
              CONDENSE lv_too NO-GAPS.
              REPLACE ALL OCCURRENCES OF lv_soo IN <lv_source> WITH lv_too.
            ENDIF.
          ENDLOOP.

          ASSIGN COMPONENT <ls_tpintnew>-fieldname OF STRUCTURE <ls_sapitab> TO FIELD-SYMBOL(<lv_target>).
          IF sy-subrc = 0.
            <lv_target> =   |{ <lv_source> ALPHA = IN }|. "The pernr of the source is converted and assigned to the real information type
          ENDIF.
        ELSE. "If this is not a personnel code
          ASSIGN COMPONENT <ls_tpintnew>-fieldname OF STRUCTURE <ls_sapitab> TO <lv_target>.
          IF sy-subrc = 0.

            IF <lv_source> IS NOT INITIAL. "The source data is not empty, you must go to the configuration table to find it once
              READ TABLE lt_tpintfind INTO DATA(ls_tpint) WITH KEY  fieldname = <ls_tpintnew>-fieldname
                                                                znsapvalue = <lv_source> "nosapvalue Now it's still the data coming in
                                                                BINARY SEARCH.
              IF sy-subrc = 0.
                ASSIGN COMPONENT <ls_tpintnew>-fieldname OF STRUCTURE <ls_sapitab> TO <lv_target>.
                IF sy-subrc = 0.


                  <lv_target> = ls_tpint-zsapvalue. "Read the matching relationship, and once there is a matching value, it must be converted

                ENDIF.
              ELSE.
*                      If no conversion value is read, it means that this data is assigned directly to the SAP field
                ASSIGN COMPONENT <ls_tpintnew>-fieldname OF STRUCTURE <ls_sapitab> TO <lv_target>.
                IF sy-subrc = 0.
                  DATA(lr_desc_type) = cl_abap_typedescr=>describe_by_data( p_data =  <lv_target> ).
                  CASE lr_desc_type->type_kind.
                    WHEN 'D'.
                      REPLACE ALL OCCURRENCES OF '-' IN <lv_source>  WITH ''.
                      CONDENSE <lv_source> NO-GAPS.
                    WHEN 'T'.
                      REPLACE ALL OCCURRENCES OF ':' IN <lv_source>  WITH ''.
                      CONDENSE <lv_source> NO-GAPS.
                  ENDCASE.

                  <lv_target> = <lv_source>.
                  CLEAR: lr_desc_type.
                ENDIF.
              ENDIF.
              CLEAR:ls_tpint.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <lv_source> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
        UNASSIGN <lv_source>.
      ENDIF.

      IF <lv_target> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
        UNASSIGN <lv_target>.
      ENDIF.
    ENDLOOP.

*          Supplement the data for the infty field
    ASSIGN COMPONENT 'INFTY' OF STRUCTURE <ls_sapitab> TO <lv_target>. "Supplement the data for the infty field
    IF sy-subrc = 0.
      <lv_target> = iv_infty.
    ENDIF.

    IF <lv_target> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
      UNASSIGN <lv_target>.
    ENDIF.

*    subtype dispose
    CALL METHOD cl_hr_t777d=>read
      EXPORTING
        infty = iv_infty
      RECEIVING
        t777d = DATA(ls_t777d).

    IF ls_t777d-namst IS NOT INITIAL.
      ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <ls_sapitab> TO <lv_target>.
      IF sy-subrc = 0.
        IF <lv_target> IS INITIAL.
          ASSIGN COMPONENT ls_t777d-namst OF STRUCTURE <ls_sapitab> TO <lv_source>.
          IF sy-subrc = 0.
            <lv_target> = <lv_source>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <lv_source> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
      UNASSIGN <lv_source>.
    ENDIF.

    IF <lv_target> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
      UNASSIGN <lv_target>.
    ENDIF.

    IF iv_infty = '0000'.
      ASSIGN COMPONENT 'RFPNR' OF STRUCTURE <ls_itab> TO <lv_source>.
      IF sy-subrc = 0.
        gv_rfpnr = <lv_source>.
      ENDIF.

      IF <lv_source> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
        UNASSIGN <lv_source>.
      ENDIF.
    ENDIF.

    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <ls_sapitab> TO <lv_target>.
    IF sy-subrc = 0.
      IF <lv_target> IS INITIAL OR <lv_target> = '00000000' .
        <lv_target> = sy-datum.
      ENDIF.
    ENDIF.
    IF <lv_target> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
      UNASSIGN <lv_target>.
    ENDIF.

    ASSIGN COMPONENT 'ENDDA' OF STRUCTURE <ls_sapitab> TO <lv_target>.
    IF sy-subrc = 0.
      IF <lv_target> IS INITIAL OR <lv_target> = '00000000' .
        <lv_target> = '99991231'.
      ENDIF.
    ENDIF.
    IF <lv_target> IS ASSIGNED. "Prevents dump from doing some dereferencing operations
      UNASSIGN <lv_target>.
    ENDIF.

  ENDLOOP.

  IF lv_messageall IS NOT INITIAL.
    APPEND INITIAL LINE TO et_message ASSIGNING FIELD-SYMBOL(<ls_message>).
    <ls_message>-msgty = 'E'.
    <ls_message>-msgtx = lv_messageall.
  ENDIF.


  CHECK lv_messageall IS INITIAL.

* If this real table is not empty, then a complete validation of the data needs to be performed
  IF <lt_sapitab> IS NOT INITIAL."
    CLEAR:lt_message.
    PERFORM frm_check_data TABLES <lt_sapitab>  "Perform a perfect validation of the data
                                  lt_tpintfind
                           USING iv_infty
                                 iv_molga
                                 iv_version
                           CHANGING lt_message.
    IF lt_message IS NOT INITIAL.
      APPEND LINES OF lt_message TO et_message. "Extract the error message of the verification
      CLEAR:lt_message.
    ELSE.
      CLEAR:lt_message.
      CASE iv_infty.
        WHEN '0000'.
          DATA(lv_infty) = 'INFTY'.
          DELETE ADJACENT DUPLICATES FROM <lt_sapitab> COMPARING (lv_infty).
          PERFORM frm_check_massn_massg TABLES <lt_sapitab> USING iv_infty CHANGING lt_message.
        WHEN '0001'.
          DELETE ADJACENT DUPLICATES FROM <lt_sapitab> COMPARING (lv_infty).
          " Check job effectiveness  Check that the job organization matches
          PERFORM frm_check_postion_valid TABLES <lt_sapitab> USING iv_infty CHANGING lt_message. "

        WHEN OTHERS.
      ENDCASE.

      APPEND LINES OF lt_message TO et_message. "Extract the error message of the verification
      CLEAR:lt_message.

*      CHECK lt_message IS INITIAL.
      "  Transform the data into a unified data format first
      LOOP AT <lt_sapitab> ASSIGNING <ls_sapitab>.
*        Convert data to a uniform format
        cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <ls_sapitab> IMPORTING prelp = DATA(ls_prelp) ).
        APPEND ls_prelp TO et_prelp.  "After skipping the data check, save it to the temporary table I need
        CLEAR:ls_prelp.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFUNCTION.
