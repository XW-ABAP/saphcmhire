*----------------------------------------------------------------------*
***INCLUDE LZXXX_HR_PAFG_ONBOARDINGF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LT_SAPITAB>  text
*      -->P_LV_TABNAME  text
*      <--P_LT_MESSAGE  text
*----------------------------------------------------------------------*
FORM frm_check_data  TABLES   it_sapitab
                              it_sapfind
                     USING    iv_infty TYPE infty
                              iv_molga TYPE viekn
                              iv_version TYPE zXXX_hr_pa_e_zversion
                     CHANGING
                               ct_message TYPE zXXX_hr_pa_tab_message.
  DATA:lv_shu TYPE i.
  DATA:lr_table TYPE REF TO data.
  DATA:lr_wa    TYPE REF TO data.
  DATA:lv_text      TYPE string.
  DATA:lv_zz        TYPE string.
  DATA:lr_descr    TYPE REF TO cl_abap_structdescr.
  DATA:lt_tpint TYPE TABLE OF zXXX_hr_pa_tpint.
  FIELD-SYMBOLS:<ls_wa> TYPE any.
  CREATE DATA lr_wa LIKE LINE OF it_sapitab.
  ASSIGN lr_wa->* TO <ls_wa>.
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
  IF sy-subrc <> 0.
    DATA(lv_tabname) = |P| && iv_infty.
    lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_tabname ).     "Extract the structure
    DATA(lt_fields) = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                              p_including_substructres = abap_true ).
  ELSE.
    lv_tabname = |P| && iv_infty.
    lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_tabname ).     "Extract the structure
    lt_fields = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                              p_including_substructres = abap_true ).

    DATA(ls_t582w) = cl_hr_t582w=>read( vinft = ls_t582v-vinft seqnr = 2 ).
    lv_tabname = |PS| && ls_t582w-infty.
    lr_descr ?= cl_abap_typedescr=>describe_by_name( p_name = lv_tabname ).     "Extract the structure
    DATA(lt_fieldssecond) = lr_descr->get_ddic_field_list( p_langu = sy-langu   "Extract the fields of the structure
                                              p_including_substructres = abap_true ).
    LOOP AT lt_fieldssecond ASSIGNING FIELD-SYMBOL(<ls_fieldssecond>).
      READ TABLE lt_fields INTO DATA(ls_fields) WITH KEY fieldname = <ls_fieldssecond>-fieldname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        <ls_fields> = <ls_fieldssecond>.
      ENDIF.
    ENDLOOP.
  ENDIF.
  lv_tabname = |P| && iv_infty.
  CLEAR:lt_tpint.
  APPEND LINES OF it_sapfind TO lt_tpint.  "  Hold lt_tpint data

  SORT lt_tpint BY  fieldname.      "Sort the data lt_tpint
  DELETE ADJACENT DUPLICATES FROM lt_tpint COMPARING fieldname.  "Remove duplicate fields
  SELECT
    infty,
    zversion,
    znumber,
    fnsource,
    sntarget
  INTO TABLE @DATA(lt_tbmove)
    FROM zXXX_hr_pa_move
    WHERE infty = @iv_infty
      AND zversion = @iv_version.
  SORT lt_tbmove BY znumber.



  IF sy-subrc = 0.
    DATA:ls_movenew LIKE LINE OF lt_tbmove.
    DATA:lt_movenew LIKE TABLE OF ls_movenew.


*      DELETE lt_tbmove WHERE znumber < 5551 OR znumber > 7000.  "Even numbers are assigned to odd numbers
    APPEND LINES OF lt_tbmove TO lt_movenew.
    DELETE lt_movenew WHERE znumber < 5551 OR znumber > 7000.
    SORT lt_movenew BY fnsource.
    DELETE ADJACENT DUPLICATES FROM lt_movenew COMPARING fnsource.

    LOOP AT it_sapitab ASSIGNING <ls_wa>.
      LOOP AT lt_tbmove ASSIGNING FIELD-SYMBOL(<ls_tbmove>).
        IF <ls_tbmove>-znumber >= 9990.  "sepcial case change data Forcibly substituting values
          ASSIGN COMPONENT <ls_tbmove>-fnsource OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<lv_source>).
          IF sy-subrc = 0.
            <lv_source> = <ls_tbmove>-sntarget.
          ENDIF.
        ELSEIF <ls_tbmove>-znumber >= 5551 AND <ls_tbmove>-znumber <= 7000. "Even numbers are assigned to odd numbers
*          lv_shu = lv_shu + 1.
*          IF  lv_shu MOD 2 = 1.
*            DATA(lv_soo) = <ls_tbmove>-sntarget.
*            CONDENSE lv_soo NO-GAPS.
*          ENDIF.
*          IF lv_shu MOD 2 = 0.
*            DATA(lv_too) = <ls_tbmove>-sntarget.
*            CONDENSE lv_too NO-GAPS.
*            REPLACE ALL OCCURRENCES OF lv_soo IN <lv_source> WITH lv_too.
*          ENDIF.
        ELSEIF <ls_tbmove>-znumber < 5551.
          ASSIGN COMPONENT <ls_tbmove>-fnsource OF STRUCTURE <ls_wa> TO <lv_source>.  "The left side of the configuration table is assigned to the right
          IF sy-subrc = 0.
            ASSIGN COMPONENT <ls_tbmove>-sntarget OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<lv_target>).
            IF sy-subrc = 0.
              <lv_target> = <lv_source>.
            ENDIF.
          ENDIF.
        ENDIF.
        IF <lv_source> IS ASSIGNED.
          UNASSIGN <lv_source>.
        ENDIF.
        IF <lv_target> IS ASSIGNED.
          UNASSIGN <lv_target>.
        ENDIF.
      ENDLOOP.
*      DELETE lt_tbmove WHERE znumber < 5551 OR znumber > 7000.  "Even numbers are assigned to odd numbers
*      APPEND LINES OF lt_tbmove TO lt_movenew.
*      SORT lt_movenew BY fnsource.
*      DELETE ADJACENT DUPLICATES FROM lt_movenew COMPARING fnsource.
      LOOP AT lt_movenew ASSIGNING FIELD-SYMBOL(<ls_mvoenew>).
        CLEAR:lv_shu.
        ASSIGN COMPONENT <ls_mvoenew>-fnsource OF STRUCTURE <ls_wa> TO <lv_source>.
        LOOP AT lt_tbmove ASSIGNING <ls_tbmove> WHERE fnsource = <ls_mvoenew>-fnsource.
          lv_shu = lv_shu + 1.
          IF  lv_shu MOD 2 = 1.
            DATA(lv_soo) = <ls_tbmove>-sntarget.
            CONDENSE lv_soo NO-GAPS.
          ELSEIF lv_shu MOD 2 = 0.
            DATA(lv_too) = <ls_tbmove>-sntarget.
            CONDENSE lv_too NO-GAPS.
            REPLACE ALL OCCURRENCES OF lv_soo IN <lv_source> WITH lv_too.
          ENDIF.
        ENDLOOP.
        IF <lv_source> IS ASSIGNED.
          UNASSIGN <lv_source>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  LOOP AT it_sapitab ASSIGNING <ls_wa>.
    DATA(lv_line) = sy-tabix.                 "Record the JSON line number
    LOOP AT lt_fields ASSIGNING <ls_fields>. "Traverse perfectly with each field
      ASSIGN COMPONENT <ls_fields>-fieldname OF STRUCTURE <ls_wa> TO FIELD-SYMBOL(<lv_field>)."Traverse perfectly with each field
      IF <lv_field> IS ASSIGNED.  " Check that the field is specified
        CASE <ls_fields>-inttype. "Determine the data type
          WHEN 'D'.
            IF <lv_field> IS NOT INITIAL.
              REPLACE ALL OCCURRENCES OF '\' IN lv_text WITH space. "Replace the special symbol '\'
              REPLACE ALL OCCURRENCES OF '-' IN lv_text WITH space. "Replace the special symbol '-'
              REPLACE ALL OCCURRENCES OF '/' IN lv_text WITH space. "Replace the special symbol '/'
              CONDENSE: lv_text NO-GAPS.
              lv_text = <lv_field> .
              lv_zz  = '^(?:(?:(?:(?:(?:1[6-9]|[2-9]\d)(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00)))(?:0229))|' &&
                      '(?:(?:(?:1[6-9]|[2-9]\d)\d{2})(?:(?:(?:0[13578]|1[02])31)|(?:(?:0[13-9]|1[0-2])(?:29|30))|(?:(?:0[1-9])|(?:1[0-2]))(' &&
                      '?:0[1-9]|1\d|2[0-8]))))$'.  "Check whether it is a date or not
              IF cl_abap_matcher=>matches(
                  pattern = lv_zz
                  text = lv_text ) NE abap_true.
                APPEND INITIAL LINE TO ct_message ASSIGNING FIELD-SYMBOL(<ls_message>).
                <ls_message>-msgty = 'E'. "If there is a problem, match it to the error message type
*The configuration table is read to obtain the source system fields and the real fields of the SAP system
                READ TABLE lt_tpint INTO DATA(ls_tpint) WITH KEY fieldname = <ls_fields>-fieldname BINARY SEARCH.
                IF sy-subrc = 0.
                  <ls_message>-msgtx = lv_tabname+1(4) && | row  | && <lv_field> &&      "Tell the other person the line number
                                       'Source system fields' && ls_tpint-znsapfield  && "Counterparty system fields
                                       'Sap system fields' && <ls_fields>-fieldname  &&  "Our system fields
                                       |The data is not in date format YYYYMMDD|.        "Returns the correct format
                ENDIF.
                CLEAR:ls_tpint.
              ENDIF.
              <lv_field> = lv_text.
            ENDIF.
          WHEN 'P'.
            IF <lv_field> IS NOT INITIAL.
              CLEAR:lv_text.
              lv_text = <lv_field>.
*              CONDENSE lv_text NO-GAPS.
              REPLACE ALL OCCURRENCES OF ',' IN lv_text WITH space.  "Replace unused symbols
              CONDENSE: lv_text NO-GAPS.          "Remove spaces
              IF cl_abap_matcher=>matches(
                  pattern =  '^[0-9]+.?[0-9]*$'
                  text = lv_text ) NE abap_true.

                APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
                <ls_message>-msgty = 'E'.

                READ TABLE lt_tpint INTO ls_tpint WITH KEY fieldname = <ls_fields>-fieldname BINARY SEARCH.
                IF sy-subrc = 0.
                  <ls_message>-msgtx = lv_tabname+1(4) && | row  | && <lv_field> &&        "Tell the other person the line number
                                       'Source system fields' && ls_tpint-znsapfield  &&   "Counterparty system fields
                                       'Sap system fields' && <ls_fields>-fieldname  &&    "Our system fields
                                       |The data is not of a numeric type|.                "Returns the correct format
                ENDIF.
                CLEAR:ls_tpint.
              ENDIF.
              <lv_field> = lv_text.
            ENDIF.
          WHEN 'N' OR 'I' .
            IF <lv_field> IS NOT INITIAL.
              CLEAR:lv_text.
              lv_text = <lv_field>.
*              CONDENSE lv_text NO-GAPS.
              REPLACE ALL OCCURRENCES OF ',' IN lv_text WITH space.
              CONDENSE: lv_text NO-GAPS.
              IF cl_abap_matcher=>matches(  "Detect whether the data is N or I
                  pattern =  '-?[0-9]\d*'
                  text = lv_text ) NE abap_true.

                APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
                <ls_message>-msgty = 'E'.

                READ TABLE lt_tpint INTO ls_tpint WITH KEY fieldname = <ls_fields>-fieldname BINARY SEARCH.
                IF sy-subrc = 0.
                  <ls_message>-msgtx = lv_tabname+1(4) && | row  | && <lv_field> &&        "Tell the other person the line number
                                       'Source system fields' && ls_tpint-znsapfield  &&   "Counterparty system fields
                                       'Sap system fields' && <ls_fields>-fieldname  &&    "Our system fields
                                       |The data is not of integer type|.                  "Returns the correct format
                ENDIF.
                CLEAR:ls_tpint.
              ENDIF.
              <lv_field> = lv_text.
            ENDIF.
          WHEN 'C'.
            DATA(lv_len) =  strlen( <lv_field> ).
            IF lv_len > <ls_fields>-leng. "Determine whether the length is satisfied
              APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
              <ls_message>-msgty = 'E'.
              READ TABLE lt_tpint INTO ls_tpint WITH KEY fieldname = <ls_fields>-fieldname BINARY SEARCH.
              IF sy-subrc = 0.
                <ls_message>-msgtx = lv_tabname+1(4) && | row  | && <lv_field> &&            "Tell the other person the line number
                                     'Source system fields' && ls_tpint-znsapfield  &&       "Counterparty system fields
                                     'Sap system fields' && <ls_fields>-fieldname  &&        "Our system fields
                                     |The data length is too long|.                          "Returns the correct format
              ENDIF.
              CLEAR:ls_tpint.
            ENDIF.
        ENDCASE.
        IF <lv_field> IS ASSIGNED.
          UNASSIGN <lv_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_RETURN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MESSAGE  text
*      -->P_<LS_ZTABLE>_INFTY  text
*      -->P_<LS_DATA>_GUID  text
*      -->P_ENDIF  text
*----------------------------------------------------------------------*
FORM frm_update_return  TABLES   it_message TYPE zXXX_hr_pa_tab_message
                                 it_return  TYPE zXXX_hr_pa_tab_return
                        USING    pv_infty  TYPE infty
                                 pv_guid  TYPE zXXX_hr_pa_e_guid.

  APPEND INITIAL LINE TO it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
  <ls_return>-guid   = pv_guid.
  <ls_return>-infty  = pv_infty.
  <ls_return>-ztable = it_message[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_POSTION_VALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LT_SAPITAB>  text
*----------------------------------------------------------------------*
FORM frm_check_postion_valid  TABLES   it_sapitab TYPE tt_p0001
                              USING    pv_infty TYPE infty
                              CHANGING ct_message TYPE zXXX_hr_pa_tab_message.
  READ TABLE it_sapitab ASSIGNING FIELD-SYMBOL(<ls_p0001>) INDEX 1.
  IF sy-subrc = 0.
    CHECK <ls_p0001> IS ASSIGNED.

    IF <ls_p0001>-plans IS INITIAL.     "Check if this is empty
      APPEND INITIAL LINE TO ct_message ASSIGNING FIELD-SYMBOL(<ls_message>).
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-002. "Position cannot be empty
      <ls_message>-fieldname = 'PLANS'.

      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_p0001>-plans AND
              znsapfield <> ''.
    ENDIF.
    SELECT SINGLE
      plvar,
      otype,
      objid,
      istat,
      begda,
      endda,
      langu,
      seqnr
      FROM hrp1000
      INTO
      @DATA(ls_p1000)
   WHERE plvar = '01' AND
         otype = 'S' AND
         objid = @<ls_p0001>-plans AND
         istat = '1' AND
         begda <= @sy-datum AND
         endda >= @sy-datum.
    IF sy-subrc NE 0.      "Check whether the position is in the validity period
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-003. "Check whether the position is in the validity period
      <ls_message>-fieldname = 'PLANS'.

      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ENDIF.
    CLEAR:ls_p1000.

    SELECT SINGLE objid FROM hrp1001 INTO @DATA(lv_orgeh) WHERE plvar = '01'
                                                     AND otype = 'O'
                                                     AND istat = '1'
                                                     AND begda <= @sy-datum
                                                     AND endda >= @sy-datum
                                                     AND rsign = 'B'
                                                     AND relat = '003'
                                                     AND sclas = 'S'
                                                     AND sobid = @<ls_p0001>-plans.
    IF sy-subrc = 0.
      <ls_p0001>-orgeh = lv_orgeh. " Automatically find organizations based on plans
    ELSE.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-004. "Organization positions do not match
      <ls_message>-fieldname = 'ORGEH'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ENDIF.
*    CLEAR:lv_orgeh.
*    IF lv_orgeh <> <ls_p0001>-orgeh.
*      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
*      <ls_message>-msgty = 'E'.
*      <ls_message>-msgtx = TEXT-004. "Organization positions do not match
*      <ls_message>-fieldname = 'ORGEH'.
*      SELECT SINGLE znsapfield
*        INTO <ls_message>-znsapfield
*        FROM zXXX_hr_pa_tpint
*        WHERE infty      = pv_infty AND
*              fieldname  = <ls_p0001>-orgeh AND
*              znsapfield <> ''.
*
*    ENDIF.


    SELECT SINGLE objid FROM hrp1001 INTO @DATA(lv_stell) WHERE plvar = '01'
                                                         AND otype = 'C'
                                                         AND istat = '1'
                                                         AND begda <= @sy-datum
                                                         AND endda >= @sy-datum
                                                         AND rsign = 'A'
                                                         AND relat = '007'
                                                         AND sclas = 'S'
                                                         AND sobid = @<ls_p0001>-plans.
    IF sy-subrc = 0.
      <ls_p0001>-stell = lv_stell.
    ELSE.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-006. "Position and Job do not match
      <ls_message>-fieldname = 'STELL'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ENDIF.
    CLEAR:lv_stell.

    SELECT SINGLE sobid FROM hrp1001 INTO @DATA(lv_kostl) WHERE plvar = '01'
                                                         AND otype = 'S'
                                                         AND istat = '1'
                                                         AND begda <= @sy-datum
                                                         AND endda >= @sy-datum
                                                         AND rsign = 'A'
                                                         AND relat = '011'
                                                         AND sclas = 'K'
                                                         AND objid = @<ls_p0001>-plans.
    IF sy-subrc = 0.
*      <ls_p0001>-kostl = lv_kostl.
    ELSE.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-016. "Position and cost center do not match
      <ls_message>-fieldname = 'KOSTL'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ENDIF.
    CLEAR:lv_kostl.

    SELECT SINGLE persa FROM hrp1008 INTO @DATA(lv_persa) WHERE plvar = '01'
                                                         AND otype = 'S'
                                                         AND istat = '1'
                                                         AND begda <= @sy-datum
                                                         AND endda >= @sy-datum
                                                         AND objid = @<ls_p0001>-plans.
    IF sy-subrc = 0.
      IF <ls_p0001>-werks IS INITIAL.
        <ls_p0001>-werks = lv_persa.
      ENDIF.
    ENDIF.
    CLEAR:lv_persa.


    SELECT SINGLE bukrs INTO @DATA(lv_bukrs)
     FROM  t500p
     WHERE persa = @<ls_p0001>-werks
       AND molga = '28'.
    IF sy-subrc = 0.
      <ls_p0001>-bukrs = lv_bukrs.
    ELSE.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-005. "Personnel Area and Company Code do not match
      <ls_message>-fieldname = 'BUKRS'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ENDIF.
    CLEAR:lv_bukrs.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FRM_CHECK_MASSN_MASSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LT_SAPITAB>  text
*      -->P_IV_INFTY  text
*      <--P_LT_MESSAGE  text
*----------------------------------------------------------------------*
FORM frm_check_massn_massg  TABLES   it_sapitab TYPE tt_p0000
                              USING    pv_infty TYPE infty
                            CHANGING ct_message TYPE zXXX_hr_pa_tab_message.

  READ TABLE it_sapitab ASSIGNING FIELD-SYMBOL(<ls_p0000>) INDEX 1.
  IF sy-subrc = 0.
    CHECK <ls_p0000> IS ASSIGNED.
    IF <ls_p0000>-massn IS INITIAL.
      APPEND INITIAL LINE TO ct_message ASSIGNING FIELD-SYMBOL(<ls_message>).
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-007. "0000 Information Type Action Type cannot be empty
      <ls_message>-fieldname = 'MASSN'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.

    ELSE.
      gv_massn = <ls_p0000>-massn.
    ENDIF.
    IF <ls_p0000>-massg IS INITIAL.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-008. "0000 Information Reason for Action cannot be empty
      <ls_message>-fieldname = 'MASSG'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ELSE.
      gv_massg = <ls_p0000>-massg.
    ENDIF.


    IF <ls_p0000>-begda IS INITIAL.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-009. "0000 Information  start date cannot be empty
      <ls_message>-fieldname = 'BEGDA'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ELSE.
      gv_begda = <ls_p0000>-begda.
    ENDIF.


    IF <ls_p0000>-pernr IS INITIAL.
      APPEND INITIAL LINE TO ct_message ASSIGNING <ls_message>.
      <ls_message>-msgty = 'E'.
      <ls_message>-msgtx = TEXT-010. "
      <ls_message>-fieldname = 'PERNR'.
      SELECT SINGLE znsapfield
        INTO <ls_message>-znsapfield
        FROM zXXX_hr_pa_tpint
        WHERE infty      = pv_infty AND
              fieldname  = <ls_message>-fieldname AND
              znsapfield <> ''.
    ELSE.
      gv_pernr = <ls_p0000>-pernr.
    ENDIF.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_PNNNN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_PRELP>  text
*----------------------------------------------------------------------*
FORM frm_update_pnnnn  USING   iv_molga TYPE viekn
                                CHANGING ps_prelp TYPE prelp.

  DATA: lv_classname  TYPE rzlli_apcl,   "Server Group Name
*      gv_guidno   type char10,
        lv_applserver TYPE rzllitab-applserver. "RFC Serve Group

  CALL 'C_SAPGPARAM'                                      "#EC CI_CCALL
         ID 'NAME'  FIELD 'rdisp/myname'
         ID 'VALUE'  FIELD lv_applserver.
  SELECT SINGLE classname
              FROM   rzllitab
              INTO   lv_classname   "Server Group Name
             WHERE   applserver = lv_applserver
               AND   grouptype = 'S'.   "S:服务器组，空:登陆组
  CALL FUNCTION 'ZXXX_HR_PAFM_PANNNN_UP' STARTING NEW TASK 'PANNNN'
    DESTINATION IN GROUP lv_classname
    EXPORTING
      is_prelp = ps_prelp
      iv_molga = iv_molga.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_UPDATE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RETURN  text
*      -->P_<LS_DATA>_GUID  text
*----------------------------------------------------------------------*
FORM frm_update_log  TABLES   it_return TYPE zXXX_hr_pa_tab_return
                     USING    pv_guid TYPE zXXX_hr_pa_e_guid
                              iv_msgty TYPE msgty
                              iv_msgtx TYPE msgtx.


  CALL METHOD /ui2/cl_json=>serialize
    EXPORTING
      data     = it_return[]
      compress = abap_true
    RECEIVING
      r_json   = DATA(lv_jsonnew).

  TRY.
      CALL METHOD cl_abap_codepage=>convert_to
        EXPORTING
          source = lv_jsonnew
*         codepage    = `UTF-8`
*         endian =
*         replacement = '#'
*         ignore_cerr = ABAP_FALSE
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

  UPDATE zXXX_hr_pa_tplog SET msgty  = iv_msgty
                              msgtx  = iv_msgtx
                              returendata = lv_dataxstring
                        WHERE guid = pv_guid.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.



ENDFORM.
