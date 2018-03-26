*&---------------------------------------------------------------------*
*& Report  ZTABLE
*&
*&---------------------------------------------------------------------*
report  ZTABLE.


*****************tablo bakım ekranı******************************

data : dba_sellist like vimsellist occurs 0 with header line,
       vname like dd02v-tabname.
data : lv_text(40) type c.
data: gt_dd03l type table of dd03l,
      gt_texto type table of char72,
      g_campo  type numc4,
      gs_texto type char72,
      g_flag   type char1.
data: v1(10) type c.
field-symbols: <fs_dd> type dd03l.
**********************************************************************
data : lv_t type c value ''.
data : tablename like databrowse-tablename.
data:    t_s_fields    like cacs_s_field      occurs 0 with header line.
selection-screen begin of block s1 with frame title text-001.
selection-screen begin of block s3 with frame title text-003.

parameters     : p_tables type DD03L-TABNAME obligatory.
parameters : p_text(40) type c visible length 40.
selection-screen end of block s3.
*parameters :  "r1 radiobutton group rad1   ,
*             r2 radiobutton group rad1 default 'X'.
selection-screen end of block s1.

at selection-screen output.
  perform add_text.

start-of-selection.
clear : v1 .
data: program(30),
      tab like textpool occurs 50 with header line.

*********************************************************************
  data : ptablename like databrowse-tablename,
        lv_tab(30) type c.
  clear : ptablename,lv_tab.
  move p_tables to ptablename.

*  if r1 is not initial.
*    v1 = 'X'.
*    export v1 to memory id 'JOB_LOCK'.
*    select tabname fieldname position keyflag
*          into corresponding fields of table gt_dd03l
*            up to 127 rows
*              from dd03l
*                where tabname  eq p_tables
*                  and rollname <> space
*                  and fieldname not like '%/%'
*                    order by position.
*
*    if sy-subrc = 0.
*      sort gt_dd03l by position.
*      loop at gt_dd03l assigning <fs_dd>.
*        at first.
*          concatenate  'Z'p_tables'_JOB_S1' into gs_texto .
*           move gs_texto to lv_tab.
*          move lv_tab to ptablename.
*          concatenate 'REPORT'  gs_texto'.' into gs_texto separated by sPACE.
*          append gs_texto to gt_texto.
*
*          concatenate 'TABLES:' p_tables '.' into gs_texto separated by space.
*          append gs_texto to gt_texto.
*          concatenate 'DATA: ITAB TYPE TABLE OF' p_tables '.' into gs_texto SEPARATED BY space.
*          append gs_texto to gt_texto.
*        endat.
*        g_campo = g_campo + 1.
*        if <fs_dd>-keyflag is initial and g_flag is initial.
*          g_flag = 'X'.
*          append 'SELECTION-SCREEN SKIP.' to gt_texto.
*        endif.
*        clear gs_texto.
*        concatenate 'SELECT-OPTIONS ' <fs_dd>-fieldname+0(7) into gs_texto SEPARATED BY space.
*        concatenate gs_texto 'FOR' into gs_texto separated by space.
*        concatenate gs_texto <fs_dd>-tabname into gs_texto separated by space.
*        concatenate gs_texto '-' <fs_dd>-fieldname into gs_texto.
*        if <fs_dd>-fieldname = 'MANDT' .
*          concatenate gs_texto 'NO-DISPLAY.' into gs_texto separated by space.
*        else.
*          concatenate gs_texto '.' into gs_texto.
*        endif.
*        append gs_texto to gt_texto.
*      endloop.
*    endif.
***********************************************************************
*
*    move 'at selection-screen output..' to gs_texto.
*     append gs_texto to gt_texto.
*     concatenate 'authority-check object' '''' into gs_texto separated bY space.
*     concatenate gs_texto 'ZTABLE_JOB' into gs_texto.
*    concatenate gs_texto '''' into gs_texto.
*    append gs_texto to gt_texto.
*
*     concatenate 'ID' '''' into gs_texto separated by space.
*     concatenate gs_texto 'BNAME' into gs_texto.
*     concatenate gs_texto '''' into gs_texto.
*    concatenate gs_texto 'field' ' SY-UNAME' into gs_texto separated by space.
*    append gs_texto to gt_texto.
*
*
*     concatenate 'ID' '''' into gs_texto separated by space.
*     concatenate gs_texto 'PROGRAMM' into gs_texto.
*     concatenate gs_texto '''' into gs_texto.
*    concatenate gs_texto 'field' ' SY-CPROG.' into gs_texto separated by space.
*    append gs_texto to gt_texto.
*
*     move 'IF SY-SUBRC NE 0.'  to gs_texto.
*    append gs_texto to gt_texto.
*
*     concatenate 'MESSAGE' '''' into gs_texto separated by space.
*     concatenate gs_texto 'programı çalıştır yetkiniz yok' into gs_texto.
*    concatenate gs_texto '''' into gs_texto.
*    append gs_texto to gt_texto.
*    concatenate 'TYPE' '''' into gs_texto separated by space.
*    concatenate gs_texto 'E''.' into gs_texto.
*    append gs_texto to gt_texto.
*    move 'ENDIF.'  to gs_texto.
*    append gs_texto to gt_texto.
*
***********************************************************************
*    append 'START-OF-SELECTION.' to gt_texto.
*    move 'data: ll_answer type c.' to gs_texto .
*    append gs_texto to gt_texto.
*    move 'data: v1(10) type c.' to gs_texto .
*    append gs_texto to gt_texto.
*    concatenate 'import v1 from memory id ' '''' into gs_texto separated BY space.
*    concatenate gs_texto 'JOB_LOCK' '''.' into gs_texto .
*    append gs_texto to gt_texto.
*    move 'if v1 is not initial.' to gs_texto.
*    append gs_texto to gt_texto.
*    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
*    concatenate gs_texto 'POPUP_TO_CONFIRM' into gs_texto.
*    concatenate gs_texto '''' into gs_texto.
*    append gs_texto to gt_texto.
*    concatenate 'exporting titlebar =' '''' into gs_texto separated by space.
*    concatenate gs_texto p_tables '''' into gs_texto .
*    append gs_texto to gt_texto.
*    concatenate 'text_question =' '''' into gs_texto separated by space.
*    concatenate gs_texto 'Do you want to delete table ?' '''' into gs_texto .
*    append gs_texto to gt_texto.
*    concatenate 'default_button =' '''' into gs_texto separated by space.
*    concatenate gs_texto '2' '''' into gs_texto .
*    append gs_texto to gt_texto.
*    concatenate 'display_cancel_button =' '''' into gs_texto separated bY space.
*    concatenate gs_texto ' ' '''' into gs_texto .
*    append gs_texto to gt_texto.
*    concatenate 'importing answer =' '' into gs_texto separated by space.
*    concatenate gs_texto ' ll_answer' '' into gs_texto separated by spacE .
*    append gs_texto to gt_texto.
*    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
*    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
*    append gs_texto to gt_texto.
*
*    move 'if ll_answer = 2.' to gs_texto.
*    append gs_texto to gt_texto.
*    move 'sy-subrc = 4. ' to gs_texto.
*    append gs_texto to gt_texto.
*    move 'check sy-subrc = 0.' to gs_texto.
*    append gs_texto to gt_texto.
*    move 'endif.' to gs_texto.
*    append gs_texto to gt_texto.
*    move 'endif.' to gs_texto.
*    append gs_texto to gt_texto.
*    concatenate 'DELETE FROM' p_tables into gs_texto separated by space.
*    append gs_texto to gt_texto.
*    concatenate  'WHERE' ' ' into gs_texto separated by space..
*    append gs_texto to gt_texto.
***********************************************************************
*    loop at gt_dd03l assigning <fs_dd>.
*      g_campo = g_campo + 1.
*      if <fs_dd>-fieldname <> 'MANDT' and <fs_dd>-fieldname <>  'Sil'.
*        concatenate <fs_dd>-fieldname 'IN' ' ' into gs_texto separated bY space.
*        concatenate ' ' gs_texto <fs_dd>-fieldname+0(7)  into gs_texto sEPARATED BY space.
*        concatenate gs_texto 'AND' into gs_texto separated by space.
*        at last.
*          concatenate <fs_dd>-fieldname 'IN' ' ' into gs_texto separated BY space.
*          concatenate ' ' gs_texto <fs_dd>-fieldname+0(7) '.' into gs_texto SEPARATED BY space.
*          append gs_texto to gt_texto.
*          exit.
*        endat.
*        append gs_texto to gt_texto.
*      endif.
*    endloop.
**    DELETE REPORT  ptablename .
*    data : lv_name like sy-uname.
*    clear : lv_name.
*    lv_name = sy-uname.
*    sy-uname = 'XTABLE_EDIT'.
*    insert report ptablename from gt_texto.
*
*
*
***********************************************************************
***********************************************************************
*    read textpool ptablename into tab language sy-langu state 'A'.
*    delete textpool ptablename language 'E'.
*    read textpool ptablename into tab language sy-langu state 'A'.
*   delete textpool ptablename language 'E'.
*   loop at gt_dd03l assigning <fs_dd>.
*   tab-id = 'S'. tab-key = <fs_dd>-fieldname .
*   tab-entry = 'Date Type'.
*   append tab.
*   endloop.
*   sort tab by id key.
*   insert textpool ptablename from tab language sy-langu.
***********************************************************************
***********************************************************************
*
*
*
*
*    submit (ptablename) via selection-screen and return.
*     sy-uname = lv_name.
*     clear : lv_name.
**    DELETE REPORT  ptablename .
*  elseif r2 is not initial.

    select tabname fieldname position keyflag
          into corresponding fields of table gt_dd03l
            up to 127 rows
              from dd03l
                where tabname  eq p_tables
                  and rollname <> space
                  and fieldname not like '%/%'
                    order by position.
    clear : ptablename.
    if sy-subrc = 0.
      sort gt_dd03l by position.
      loop at gt_dd03l assigning <fs_dd>.
        at first.

          concatenate  'Z'p_tables'_MANUEL_S1' into gs_texto .
           move gs_texto to lv_tab.
          move lv_tab to ptablename.
          concatenate 'REPORT'  gs_texto'.' into gs_texto separated by sPACE.
          append gs_texto to gt_texto.


          concatenate 'TABLES:' p_tables '.' into gs_texto separated by space.
          append gs_texto to gt_texto.
          concatenate 'DATA: ITAB TYPE TABLE OF' p_tables '.' into gs_texto SEPARATED BY space.
          append gs_texto to gt_texto.
**********************************************************************
          move 'type-pools: rsds.' to gs_texto .
          append gs_texto to gt_texto.
          move 'data: l_answer type c,l_eflag type c.' to gs_texto .
          append gs_texto to gt_texto.
**
          move 'data: is_x030l type x030l,it_dfies type table of dfies,' TO gs_texto.
          append gs_texto to gt_texto.
          move  'is_dfies type dfies,it_fdiff type table of field_dif,' TO gs_texto.
          append gs_texto to gt_texto.
          move  'is_fdiff type field_dif.' to gs_texto.
          append gs_texto to gt_texto.
*
*
          move 'data: w_selid type rsdynsel-selid,' to gs_texto.
          append gs_texto to gt_texto.
          move 'it_tables type table of rsdstabs,' to gs_texto.
          append gs_texto to gt_texto.
          move 'is_tables type rsdstabs,' to gs_texto.
          append gs_texto to gt_texto.
          move 'it_fields type table of rsdsfields,' to gs_texto.
          append gs_texto to gt_texto.
          move 'it_expr type rsds_texpr,' to gs_texto.
          append gs_texto to gt_texto.
          move 'it_ranges type rsds_trange,' to gs_texto.
          append gs_texto to gt_texto.
          move 'it_where type rsds_twhere,' to gs_texto.
          append gs_texto to gt_texto.
          move 'is_where type rsds_where,w_active type i.' to gs_texto.
          append gs_texto to gt_texto.
***
          move 'data: it_content type ref to data,it_modif type ref to data,' TO gs_texto.
          append gs_texto to gt_texto.
          move 'it_fcat type lvc_t_fcat.' to gs_texto.
          append gs_texto to gt_texto.
**
          move 'data: w_okcode type sy-ucomm.' to gs_texto.
          append gs_texto to gt_texto.

          move 'field-symbols: <itab1> type standard table,' to gs_texto.
          append gs_texto to gt_texto.
          move '<ntab> type standard table.' to gs_texto.
          append gs_texto to gt_texto.
*
          move 'field-symbols: <fcat> type lvc_s_fcat.' to gs_texto.
          append gs_texto to gt_texto.


**********************************************************************
        endat.


        g_campo = g_campo + 1.
        if <fs_dd>-keyflag is initial and g_flag is initial.
          g_flag = 'X'.
          append 'SELECTION-SCREEN SKIP.' to gt_texto.
        endif.
        clear gs_texto.
        concatenate 'SELECT-OPTIONS ' <fs_dd>-fieldname+0(7) into gs_texto SEPARATED BY space.
        concatenate gs_texto 'FOR' into gs_texto separated by space.
        concatenate gs_texto <fs_dd>-tabname into gs_texto separated by space.
        concatenate gs_texto '-' <fs_dd>-fieldname into gs_texto.
        if <fs_dd>-fieldname = 'MANDT' .
          concatenate gs_texto 'NO-DISPLAY.' into gs_texto separated by space.
        else.
          concatenate gs_texto '.' into gs_texto.
        endif.
        append gs_texto to gt_texto.
      endloop.
    endif.



*    move 'at selection-screen output..' to gs_texto.
*     append gs_texto to gt_texto.
*     concatenate 'authority-check object' '''' into gs_texto separated bY space.
*     concatenate gs_texto 'ZTABLE_JOB' into gs_texto.
*    concatenate gs_texto '''' into gs_texto.
*    append gs_texto to gt_texto.
*
*     concatenate 'ID' '''' into gs_texto separated by space.
*     concatenate gs_texto 'BNAME' into gs_texto.
*     concatenate gs_texto '''' into gs_texto.
*    concatenate gs_texto 'field' ' SY-UNAME' into gs_texto separated by space.
*    append gs_texto to gt_texto.
*
*
*     concatenate 'ID' '''' into gs_texto separated by space.
*     concatenate gs_texto 'PROGRAMM' into gs_texto.
*     concatenate gs_texto '''' into gs_texto.
*    concatenate gs_texto 'field' ' SY-CPROG.' into gs_texto separated by space.
*    append gs_texto to gt_texto.
*
*     move 'IF SY-SUBRC NE 0.'  to gs_texto.
*    append gs_texto to gt_texto.
*
*     concatenate 'MESSAGE' '''' into gs_texto separated by space.
*     concatenate gs_texto 'programı çalıştır yetkiniz yok' into gs_texto.
*    concatenate gs_texto '''' into gs_texto.
*    append gs_texto to gt_texto.
*    concatenate 'TYPE' '''' into gs_texto separated by space.
*    concatenate gs_texto 'E''.' into gs_texto.
*    append gs_texto to gt_texto.
*    move 'ENDIF.'  to gs_texto.
*    append gs_texto to gt_texto.

*********************************************************************
    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'LVC_FIELDCATALOG_MERGE' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'EXPORTING i_structure_name =' '''' into gs_texto separaTED BY space.
    concatenate gs_texto p_tables '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'changing ct_fieldcat = ' '' into gs_texto separated by space.
    concatenate gs_texto ' it_fcat' '' into  gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto.
    append gs_texto to gt_texto.

    move 'IF SY-SUBRC = 0.' to gs_texto.
    append gs_texto to gt_texto.
    move 'LOOP AT it_fcat ASSIGNING <fcat>.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate '<fcat>-tabname = ' ''''   into gs_texto separated by space.
    concatenate gs_texto p_tables  '''.'   into gs_texto separated by space.
    append gs_texto to gt_texto.
    move 'ENDLOOP.' to gs_texto.
    append gs_texto to gt_texto.



    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'LVC_FIELDCAT_COMPLETE' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'CHANGING ct_fieldcat =' '' into gs_texto separated by space.
    concatenate gs_texto ' it_fcat' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
    append gs_texto to gt_texto.

    move 'ENDIF.'to gs_texto.
    append gs_texto to gt_texto.
***
    concatenate 'CALL METHOD' '' into gs_texto separated by space.
    concatenate gs_texto ' CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE' inTO gs_texto SEPARATED BY space.
    concatenate gs_texto '' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'EXPORTING it_fieldcatalog =' '' into gs_texto separated BY space.
    concatenate gs_texto ' it_fcat' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
**
    concatenate 'IMPORTING ep_table =' '' into gs_texto separated by space.
    concatenate gs_texto ' it_content.' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
**
    move 'IF sy-subrc = 0.'to gs_texto.
    append gs_texto to gt_texto.
    move 'ASSIGN it_content->* TO <itab1>.' to gs_texto.
    append gs_texto to gt_texto.
    move 'ENDIF.' to gs_texto.
    append gs_texto to gt_texto.
**
    concatenate 'CALL METHOD' '' into gs_texto separated by space.
    concatenate gs_texto ' cl_alv_table_create=>create_dynamic_table' inTO gs_texto SEPARATED BY space.
    concatenate gs_texto '' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'EXPORTING it_fieldcatalog =' '' into gs_texto separated BY space.
    concatenate gs_texto ' it_fcat' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
**
    concatenate 'IMPORTING ep_table =' '' into gs_texto separated by space.
    concatenate gs_texto ' it_modif.' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
*
    move 'IF sy-subrc = 0.'to gs_texto.
    append gs_texto to gt_texto.
    move 'ASSIGN it_modif->* TO <ntab>.' to gs_texto.
    append gs_texto to gt_texto.
    move 'ENDIF.' to gs_texto.
    append gs_texto to gt_texto.

    concatenate 'SELECT * FROM' p_tables into gs_texto separated by space.
    append gs_texto to gt_texto.
    clear : gs_texto.
    concatenate gs_texto 'up to 50 rows' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'INTO CORRESPONDING FIELDS OF TABLE <itab1>' 'WHERE' intO gs_texto SEPARATED BY space..
    append gs_texto to gt_texto.
    clear g_campo.
    loop at gt_dd03l assigning <fs_dd>.
      g_campo = g_campo + 1.
      if <fs_dd>-fieldname <> 'MANDT' and <fs_dd>-fieldname <>  'Sil'.
        concatenate <fs_dd>-fieldname 'IN' ' ' into gs_texto separated bY space.
        concatenate ' ' gs_texto <fs_dd>-fieldname+0(7)  into gs_texto sEPARATED BY space.
        concatenate gs_texto 'AND' into gs_texto separated by space.
        at last.
          concatenate <fs_dd>-fieldname 'IN' ' ' into gs_texto separated BY space.
          concatenate ' ' gs_texto <fs_dd>-fieldname+0(7) '.' into gs_texto SEPARATED BY space.
          append gs_texto to gt_texto.
          exit.
        endat.
        append gs_texto to gt_texto.
      endif.
    endloop.

    move 'CLEAR: w_okcode.' to gs_texto.
    append gs_texto to gt_texto.
    move 'REFRESH: <ntab>.' to gs_texto.
    append gs_texto to gt_texto.

    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'STC1_FULLSCREEN_TABLE_CONTROL' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.

    concatenate 'EXPORTING HEADER =' '''' into gs_texto separated by space.
    concatenate gs_texto p_tables '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'tabname =' '''' into gs_texto separated by space.
    concatenate gs_texto p_tables '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'display_only =' '''' into gs_texto separated by space.
    concatenate gs_texto ' ' '''' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'endless =' '''' into gs_texto separated by space.
    concatenate gs_texto  'X' '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'no_button =' '' into gs_texto separated by space.
    concatenate gs_texto  'SPACE' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
*
    concatenate 'IMPORTING okcode =' '' into gs_texto separated by space.
    concatenate gs_texto  'w_okcode' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'TABLES nametab =' '' into gs_texto separated by space.
    concatenate gs_texto  'it_dfies' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'table =' '' into gs_texto separated by space.
    concatenate gs_texto  '<itab1>' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'fielddif =' '' into gs_texto separated by space.
    concatenate gs_texto  'it_fdiff' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'modif_table =' '' into gs_texto separated by space.
    concatenate gs_texto  '<ntab>' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    move 'if sy-subrc = 0.' to gs_texto.
    append gs_texto to gt_texto.

    concatenate ' if w_okcode  =' '''' into gs_texto separated by space.
    concatenate gs_texto  'SAVE' '''.' into gs_texto .
    append gs_texto to gt_texto.

    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'POPUP_TO_CONFIRM' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'exporting titlebar =' '''' into gs_texto separated by space.
    concatenate gs_texto p_tables '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'text_question =' '''' into gs_texto separated by space.
    concatenate gs_texto 'Do you want to update table ?' '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'default_button =' '''' into gs_texto separated by space.
    concatenate gs_texto '2' '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'display_cancel_button =' '''' into gs_texto separated bY space.
    concatenate gs_texto ' ' '''' into gs_texto .
    append gs_texto to gt_texto.
    concatenate 'importing answer =' '''' into gs_texto separated by space.
    concatenate gs_texto 'answer' '''' into gs_texto .
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    move 'if not <ntab>[] is initial.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate   'modify ' p_tables ' from table <ntab>.'
    into gs_texto separated by space.
    append gs_texto to gt_texto.
    move  'if sy-subrc ne 0.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate 'l_eflag =' '''' into gs_texto separated by space.
    concatenate gs_texto 'X' '''.' into gs_texto .
    append gs_texto to gt_texto.

    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.

**********************************************************************
    move 'if l_eflag is initial.' to gs_texto.
    append gs_texto to gt_texto.
    move 'refresh: <ntab>.' to gs_texto.
    append gs_texto to gt_texto.

    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'STC1_GET_DATA' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'tables deleted_data =' '' into gs_texto separated by space.
    concatenate gs_texto '<ntab>' '' into gs_texto separated by space..
    append gs_texto to gt_texto.
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
    append gs_texto to gt_texto.

    move 'if not <ntab>[] is initial.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate   'delete ' p_tables ' from table <ntab>.'
    into gs_texto separated by space.
    append gs_texto to gt_texto.
    move  'if sy-subrc ne 0.' to gs_texto.
    append gs_texto to gt_texto.
    move 'rollback work.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate 'l_eflag =' '''' into gs_texto separated by space.
    concatenate gs_texto 'X' '''.' into gs_texto .
    append gs_texto to gt_texto.

    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
**********************************************************************

    move 'if l_eflag is initial.' to gs_texto.
    append gs_texto to gt_texto.
    move 'refresh: <ntab>.' to gs_texto.
    append gs_texto to gt_texto.


    concatenate 'CALL FUNCTION' '''' into gs_texto separated by space.
    concatenate gs_texto 'STC1_GET_DATA' into gs_texto.
    concatenate gs_texto '''' into gs_texto.
    append gs_texto to gt_texto.
    concatenate 'tables new_data =' '' into gs_texto separated by space.
    concatenate gs_texto '<ntab>' '' into gs_texto separated by space.
    append gs_texto to gt_texto.
    concatenate 'EXCEPTIONS OTHERS =' '' into gs_texto separated by space.
    concatenate gs_texto ' 1. ' '' into gs_texto separated by space.
    append gs_texto to gt_texto.


    move 'if not <ntab>[] is initial.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate   'insert ' p_tables ' from table <ntab>.'
    into gs_texto separated by space.
    append gs_texto to gt_texto.
    move  'if sy-subrc ne 0.' to gs_texto.
    append gs_texto to gt_texto.
    move 'rollback work.' to gs_texto.
    append gs_texto to gt_texto.
    concatenate 'l_eflag =' '''' into gs_texto separated by space.
    concatenate gs_texto 'X' '''.' into gs_texto .
    append gs_texto to gt_texto.


    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.


    move 'if l_eflag is initial.' to gs_texto.
    append gs_texto to gt_texto.
    move 'commit work.' to gs_texto.
    append gs_texto to gt_texto.
    move  'message s261(53).' to gs_texto.
    append gs_texto to gt_texto.
    move 'else.' to gs_texto.
    append gs_texto to gt_texto.
    move 'message s075(3i).' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    move 'endif.' to gs_texto.
    append gs_texto to gt_texto.
    delete report  ptablename .
    insert report ptablename from gt_texto.
**********************************************************************
**********************************************************************
    read textpool ptablename into tab language sy-langu state 'A'.
    delete textpool ptablename language 'E'.
*    if r3 is  initial OR r4 is  initial.
    read textpool ptablename into tab language sy-langu state 'A'.
   delete textpool ptablename language 'E'.
   loop at gt_dd03l assigning <fs_dd>.
   tab-id = 'S'. tab-key = <fs_dd>-fieldname .
   tab-entry = 'Date Type'.
   append tab.
   endloop.
   sort tab by id key.
   insert textpool ptablename from tab language sy-langu.
*   ENDIF.
**********************************************************************
**********************************************************************
    submit (ptablename) via selection-screen and return.
    delete report ptablename.

*  endif.

*&---------------------------------------------------------------------*
*&      Form  add_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_tables .
*  CASE sy-ucomm.
*    WHEN 'UPS'.
*      vname = 'ZSD_MOBIS_TABLES' .
*      REFRESH dba_sellist.
*      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
*        EXPORTING
*          action                       = 'S'
*          show_selection_popup         = ' '
*          view_name                    = vname
*          variant_for_selection        = ' '
*        TABLES
*          dba_sellist                  = dba_sellist
*        EXCEPTIONS
*          client_reference             = 1
*          foreign_lock                 = 2
*          invalid_action               = 3
*          no_clientindependent_auth    = 4
*          no_database_function         = 5
*          no_editor_function           = 6
*          no_show_auth                 = 7
*          no_tvdir_entry               = 8
*          no_upd_auth                  = 9
*          only_show_allowed            = 10
*          system_failure               = 11
*          unknown_field_in_dba_sellist = 12
*          view_not_found               = 13
*          OTHERS                       = 14.
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*
*  ENDCASE .
endform.                    " add_tables
*&---------------------------------------------------------------------*
*&      Form  add_text
*&---------------------------------------------------------------------*

form add_text .
  clear : p_text.
  select single ddtext
    from dd02t
    into p_text
    where tabname eq p_tables and
          ddlanguage eq sy-langu.
  loop at screen.
    if screen-name = 'P_TEXT'.
      screen-input = 'X'.
      screen-display_3d = 'X'.
      modify screen.
    endif.
  endloop.
endform.                    " add_text
