CLASS zcl_abapchain_log_reader DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS print_range
      IMPORTING
        object              TYPE if_bali_log_filter=>ty_object    DEFAULT 'ZABAPCHAIN'
        subobject           TYPE if_bali_log_filter=>ty_subobject DEFAULT 'AGENT_RUN'
        external_id_pattern TYPE if_bali_log_filter=>ty_external_id OPTIONAL
        date_from           TYPE d
        time_from           TYPE t DEFAULT '000000'
        date_to             TYPE d
        time_to             TYPE t DEFAULT '235959'
        max_logs            TYPE if_bali_log_filter=>ty_max_log_number DEFAULT 15
        out                 TYPE REF TO if_oo_adt_classrun_out
      RAISING
        cx_bali_runtime.

  PRIVATE SECTION.
    CLASS-METHODS to_utclong
      IMPORTING
        date      TYPE d
        time      TYPE t
      RETURNING
        VALUE(ts) TYPE if_bali_log_filter=>ty_timestamp.


    CLASS-METHODS write_one_log
      IMPORTING
        log TYPE REF TO if_bali_log
        out TYPE REF TO if_oo_adt_classrun_out.
ENDCLASS.


CLASS zcl_abapchain_log_reader IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA(today) = cl_abap_context_info=>get_system_date( ).
    DATA yesterday TYPE d.

    yesterday = today - 1.

    TRY.
        print_range(
          date_from = yesterday
          date_to   = today
          max_logs  = 20
          out       = out ).
      CATCH cx_bali_runtime INTO DATA(err).
        out->write( |BALI read error: { err->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.


  METHOD print_range.
    "1) Build filter
    DATA(filter) = cl_bali_log_filter=>create( ).

    IF external_id_pattern IS INITIAL.
      filter = filter->set_descriptor(
                 object    = object
                 subobject = subobject ).
    ELSE.
      filter = filter->set_descriptor(
                 object      = object
                 subobject   = subobject
                 external_id = external_id_pattern ).
    ENDIF.

    filter = filter->set_maximum_log_number(
               max_log_number = max_logs ).


    "2) Convert local date/time range to UTCLONG UTC timestamps
    "   (Filter internally converts UTCLONG -> date/time in system timezone, see your class)
    DATA(start_utc) = to_utclong( date = date_from time = time_from ).
    DATA(end_utc)   = to_utclong( date = date_to   time = time_to ).

    filter = filter->set_time_interval(
               start_time = start_utc
               end_time   = end_utc ).

    " for deubug only! remove !
    "filter = cl_bali_log_filter=>create( ).
    "filter = filter->set_maximum_log_number( max_log_number = 1 ).

    TRY.
        DATA(log_db) = cl_bali_log_db=>get_instance( ).
*        DATA(logs)   = log_db->load_logs_via_filter( filter ).
        DATA(logs) = log_db->load_logs_w_items_via_filter( filter ).

      CATCH cx_bali_runtime INTO DATA(lx_bali).
        out->write( |cx_bali_runtime: { lx_bali->get_text( ) }| ).

        "Often contains the real error reason (auth / invalid filter / DB issue)
        IF lx_bali->previous IS BOUND.
          out->write( |Previous: { lx_bali->previous->get_text( ) }| ).
        ENDIF.

        "If it's a T100-based message
        TRY.
            DATA(t100) = CAST if_t100_message( lx_bali ).
            out->write( |T100: { t100->t100key-msgid } { t100->t100key-msgno }| ).
          CATCH cx_sy_move_cast_error.
        ENDTRY.

        RAISE EXCEPTION lx_bali.
    ENDTRY.


    IF logs IS INITIAL.
      out->write( |No logs found for { object }/{ subobject } in the given range.| ).
      RETURN.
    ENDIF.



    LOOP AT logs INTO DATA(log).
      write_one_log( log = log out = out ).
    ENDLOOP.
  ENDMETHOD.


  METHOD to_utclong.
    DATA(timezone) = cl_abap_tstmp=>get_system_timezone( ).
    CONVERT DATE date TIME time TIME ZONE timezone INTO UTCLONG ts.
  ENDMETHOD.


  METHOD write_one_log.
    out->write( |--- Application Log ---| ).
    out->write( |Handle={ log->get_handle( ) }| ).

    DATA(items) = log->get_all_items( ).
    IF items IS INITIAL.
      out->write( |<no items>| ).
      out->write( |--- End of Log ---| ).
      RETURN.
    ENDIF.

    "Use first item as a pseudo-header (your logger writes RUN START first)
    READ TABLE items INDEX 1 INTO DATA(first).
    IF sy-subrc = 0.
      out->write( |Start: { first-item->get_message_text( ) }| ).
    ENDIF.

    LOOP AT items INTO DATA(row).
      out->write( row-item->get_message_text( ) ).
    ENDLOOP.

    out->write( |--- End of Log ---| ).
  ENDMETHOD.

ENDCLASS.
