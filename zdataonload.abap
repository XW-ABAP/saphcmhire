FUNCTION zXXX_hr_pafm_dataonload.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_JSON) TYPE  STRING
*"     VALUE(IV_VERSION) TYPE  ZXXX_HR_PA_E_ZVERSION OPTIONAL
*"     VALUE(IV_MOLGA) TYPE  VIEKN DEFAULT '28'
*"  EXPORTING
*"     VALUE(EV_JSON) TYPE  STRING
*"----------------------------------------------------------------------

  DATA:lt_hirereturn   TYPE hrpad_return_tab,
       lt_hirepakeytab TYPE hrpad_bapipakey_tab,
       lv_ok           TYPE boole_d.
  DATA:ls_p0006 TYPE p0006,
       ls_p0022 TYPE p0022.
  DATA lv_msgty TYPE msgty.
  DATA:lv_msgtx TYPE msgtx.
  TYPES: BEGIN OF ts_s_data,
           guid TYPE zXXX_hr_pa_e_guid.
  TYPES:
   ztable TYPE zXXX_hr_pa_tab_data.
  TYPES:       END OF ts_s_data.

  TYPES:tt_s_data TYPE TABLE OF ts_s_data.
  FIELD-SYMBOLS:<lt_itab> TYPE STANDARD TABLE,
                <ls_itab> TYPE any.
  FIELD-SYMBOLS:<lt_sapitab> TYPE STANDARD TABLE,
                <ls_sapitab> TYPE any.
  DATA:lv_tabname TYPE tabname.
  DATA:lt_prelp TYPE TABLE OF prelp.
  DATA:lt_message TYPE zXXX_hr_pa_tab_message.

  DATA:lt_data TYPE  tt_s_data.
  DATA:lv_flag TYPE flag.
  DATA:lt_return TYPE zXXX_hr_pa_tab_return.
  DATA:lt_tplog TYPE TABLE OF zXXX_hr_pa_tplog.
  DATA:lt_zXXX_hr_pa_tab_message TYPE zXXX_hr_pa_tab_message.
  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = iv_json
    CHANGING
      data = lt_data.
  CLEAR:gt_return.
  CHECK lt_data IS NOT INITIAL.
*******  Here you need to add a code to verify it
*        Whether this GUID has been successful, do not continue to execute if it is successful
*******  Here you need to add a code to verify it
  SELECT
         guid,
         msgty,
         msgtx
     FROM zXXX_hr_pa_tplog
     INTO TABLE @DATA(lt_tplogfind)
     FOR ALL ENTRIES IN @lt_data
     WHERE guid = @lt_data-guid AND
           msgty = 'S'.
  IF sy-subrc = 0.
    lv_msgtx = TEXT-015.
    LOOP AT lt_tplogfind  ASSIGNING FIELD-SYMBOL(<ls_tplogfind>).
      APPEND INITIAL LINE TO gt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      <ls_return>-guid = <ls_tplogfind>-guid.
      APPEND INITIAL LINE TO <ls_return>-ztable ASSIGNING FIELD-SYMBOL(<ls_ztablereturn>).
      <ls_ztablereturn>-fieldname = 'GUID'.
      <ls_ztablereturn>-znsapfield = 'GUID'.
      <ls_ztablereturn>-msgty = 'E'.
      <ls_ztablereturn>-msgtx = lv_msgtx.
    ENDLOOP.
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = gt_return
      RECEIVING
        r_json = ev_json.
    RETURN.
  ENDIF.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data   = <ls_data>
      RECEIVING
        r_json = DATA(lv_jsonnew).
    TRY.
        CALL METHOD cl_abap_codepage=>convert_to
          EXPORTING
            source = lv_jsonnew
*           codepage    = `UTF-8`
*           endian =
*           replacement = '#'
*           ignore_cerr = ABAP_FALSE
          RECEIVING
            result = DATA(lv_dataxstring).
      CATCH cx_parameter_invalid_range INTO DATA(lr_invalid_range).
        DATA(lv_message) = lr_invalid_range->get_text( ).
      CATCH cx_sy_codepage_converter_init INTO DATA(lr_init).
        lv_message = lr_init->get_text( ).
      CATCH cx_sy_conversion_codepage INTO DATA(lr_codepage).
        lv_message = lr_codepage->get_text( ).
      CATCH cx_parameter_invalid_type INTO DATA(lr_invalid_type).
        lv_message = lr_invalid_type->get_text( ).
    ENDTRY.
    "
    APPEND INITIAL LINE TO lt_tplog ASSIGNING FIELD-SYMBOL(<ls_tplog>).
    <ls_tplog>-guid = <ls_data>-guid.
    <ls_tplog>-sdate = sy-datum.
    <ls_tplog>-stime = sy-uzeit.
    <ls_tplog>-data = lv_dataxstring.
    CLEAR:lv_dataxstring.
  ENDLOOP.

  IF lt_tplog IS NOT INITIAL.
    MODIFY zXXX_hr_pa_tplog FROM TABLE lt_tplog.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
  ENDIF.

  IF iv_version IS INITIAL.
    iv_version = 'SAP'.
  ENDIF.

  SELECT
    mandt,
    infty,
    zversion,
    fieldname,
    znumber,
    zsapvalue,
    znsapfield,
    znsapvalue
    FROM
    zXXX_hr_pa_tpint
    INTO TABLE @DATA(lt_tpint)
    WHERE infty      <> '' AND
          znsapfield <> '' AND
          zversion   = @iv_version.

  LOOP AT  lt_data ASSIGNING <ls_data>.
    "A GUID corresponds to a person data
    CLEAR:lt_return,
          gv_massn,
          gv_massg,
          gv_rfpnr,
          gv_pernr,
          gv_begda,
          lt_prelp.

    LOOP AT <ls_data>-ztable ASSIGNING FIELD-SYMBOL(<ls_ztable>).
*      One type of information to get once
      CLEAR:lt_message.

      CALL FUNCTION 'ZXXX_HR_PAFM_DATATOPRELP_V0001'
        EXPORTING
          iv_infty   = <ls_ztable>-infty
          iv_data    = <ls_ztable>-data
          iv_molga   = iv_molga
          iv_version = iv_version
        IMPORTING
          et_prelp   = lt_prelp
          et_message = lt_message
        TABLES
          it_tpint   = lt_tpint.
      IF lt_message IS NOT INITIAL.
        PERFORM frm_update_return  TABLES lt_message
                                          lt_return
                                   USING <ls_ztable>-infty
                                         <ls_data>-guid.
      ENDIF.
    ENDLOOP.

    IF lt_return IS NOT INITIAL.
      APPEND LINES OF lt_return TO gt_return.
      CLEAR:lt_return.
      EXIT.
    ENDIF.

    DELETE lt_prelp WHERE infty = '0000'.

    lv_flag = ''.

    CLEAR:lt_hirereturn,
          lt_hirepakeytab,
          lv_ok.

    IF gv_pernr IS NOT INITIAL.
      SELECT SINGLE pernr INTO @DATA(lv_pernr)
        FROM pa0000
        WHERE pernr = @gv_pernr AND
              stat2 = '3' AND
              begda <= @sy-datum AND
              endda >= @sy-datum.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO gt_return ASSIGNING <ls_return>.
        <ls_return>-guid = <ls_data>-guid.
        APPEND INITIAL LINE TO <ls_return>-ztable ASSIGNING <ls_ztablereturn>.
        <ls_ztablereturn>-fieldname = 'PERNR'.
        <ls_ztablereturn>-znsapfield = ''.
        <ls_ztablereturn>-msgty = 'E'.
        <ls_ztablereturn>-msgtx = gv_pernr && |  | && TEXT-012.


        lv_msgty = 'E'.
        lv_msgtx = TEXT-013.

        PERFORM frm_update_log TABLES gt_return
                               USING <ls_data>-guid
                                     lv_msgty
                                     lv_msgtx.

        CALL METHOD /ui2/cl_json=>serialize
          EXPORTING
            data   = gt_return
          RECEIVING
            r_json = ev_json.


        CONTINUE.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'HR_PAD_HIRE_EMPLOYEE'
      EXPORTING
        employeenumber  = gv_pernr
*       referencepernr  = gv_rfpnr
        hiringdate      = gv_begda
        actiontype      = gv_massn
        reasonforaction = gv_massg
        pnnnn_tab       = lt_prelp
*       PREF_TAB        =
        nocommit        = lv_flag
*       IT_HIRE_FIELDNAMES       =
      IMPORTING
        return_tab      = lt_hirereturn
        bapipakey_tab   = lt_hirepakeytab
        is_ok           = lv_ok.

    IF lv_ok IS INITIAL.
      APPEND INITIAL LINE TO gt_return ASSIGNING <ls_return>.
      <ls_return>-guid = <ls_data>-guid.
      LOOP AT lt_hirereturn ASSIGNING FIELD-SYMBOL(<ls_hirereturn>).
        APPEND INITIAL LINE TO <ls_return>-ztable ASSIGNING <ls_ztablereturn>.
        MESSAGE ID <ls_hirereturn>-id TYPE <ls_hirereturn>-type  NUMBER <ls_hirereturn>-number
        WITH <ls_hirereturn>-message_v1
            <ls_hirereturn>-message_v2
            <ls_hirereturn>-message_v3
            <ls_hirereturn>-message_v4 INTO <ls_ztablereturn>-msgtx.
        <ls_ztablereturn>-msgty = 'E'.
      ENDLOOP.

      lv_msgty = 'E'.
      lv_msgtx = TEXT-013.

      PERFORM frm_update_log TABLES gt_return
                             USING <ls_data>-guid
                                   lv_msgty
                                   lv_msgtx.
    ELSE.
      READ TABLE lt_hirepakeytab INTO DATA(ls_hirepakeytab) INDEX 1.
      IF sy-subrc = 0.
*       ls_hirepakeytab-employeeno.
        APPEND INITIAL LINE TO gt_return ASSIGNING <ls_return>.
        <ls_return>-guid = <ls_data>-guid.
        APPEND INITIAL LINE TO <ls_return>-ztable ASSIGNING <ls_ztablereturn>.
        <ls_ztablereturn>-fieldname = 'PERNR'.
        <ls_ztablereturn>-znsapfield = ''.
        <ls_ztablereturn>-msgty = 'S'.
        <ls_ztablereturn>-msgtx = ls_hirepakeytab-employeeno && |  | && TEXT-011.

        lv_msgty = 'S'.
        lv_msgtx = TEXT-014.

        PERFORM frm_update_log TABLES gt_return
                               USING <ls_data>-guid
                                     lv_msgty
                                     lv_msgtx.
      ENDIF.
      SELECT
        infty,
        zversion,
        flag
      FROM zXXX_hr_pa_tpinf
        WHERE zversion = @iv_version
          AND flag <> ''
      INTO TABLE @DATA(lt_tpinf).
      IF sy-subrc = 0.
        LOOP AT lt_tpinf ASSIGNING FIELD-SYMBOL(<ls_tpinf>).
          LOOP AT lt_prelp ASSIGNING FIELD-SYMBOL(<ls_prelp>) WHERE infty = <ls_tpinf>-infty.
            PERFORM frm_update_pnnnn USING  iv_molga CHANGING <ls_prelp>.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF gt_return[] IS NOT INITIAL.
      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = gt_return
        RECEIVING
          r_json = ev_json.
    ENDIF.

  ENDLOOP.
ENDFUNCTION.
