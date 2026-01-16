CLASS lhc_HitlReq DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR HitlReq RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR HitlReq RESULT result.

    METHODS Respond FOR MODIFY
      IMPORTING
        keys FOR ACTION HitlReq~Respond.

    METHODS set_created_audit FOR DETERMINE ON MODIFY
      IMPORTING keys FOR HitlReq~set_created_audit.
    METHODS set_responded_meta FOR DETERMINE ON MODIFY
      IMPORTING keys FOR HitlReq~set_responded_meta.


ENDCLASS.

CLASS lhc_HitlReq IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD Respond.

    DATA current_timestamp TYPE timestampl.
    GET TIME STAMP FIELD current_timestamp.

    DATA current_user TYPE syuname.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      MODIFY ENTITIES OF zc_ai_hitl_req IN LOCAL MODE
        ENTITY HitlReq
        UPDATE FIELDS ( Status ResponsePayload )
        WITH VALUE #(
          ( %tky           = <k>-%tky
            Status         = 'RESPONDED'
            ResponsePayload = <k>-%param-ResponsePayload
*            RespondedAt   = current_timestamp
*            RespondedBy   = current_user

*            %control-Status        = if_abap_behv=>mk-on
*            %control-ResponsePayload = if_abap_behv=>mk-on
*            %control-RespondedAt   = if_abap_behv=>mk-on
*            %control-RespondedBy   = if_abap_behv=>mk-on

            )
        )
        FAILED DATA(failed_local)
        REPORTED DATA(reported_local).

    ENDLOOP.

  ENDMETHOD.

  METHOD set_created_audit.
    DATA current_timestamp TYPE timestampl.
    GET TIME STAMP FIELD current_timestamp.

    MODIFY ENTITIES OF zc_ai_hitl_req IN LOCAL MODE
      ENTITY HitlReq
      UPDATE FIELDS ( CreatedAt CreatedBy )
      WITH VALUE #(
        FOR k IN keys (
          %key      = k-%key
          CreatedAt = current_timestamp
          CreatedBy = sy-uname
        )
      ).
  ENDMETHOD.



  METHOD set_responded_meta.

  DATA current_time TYPE timestampl.
  DATA current_user TYPE syuname.
  GET TIME STAMP FIELD current_time.
  current_user = cl_abap_context_info=>get_user_technical_name( ).

  " Read changed instances
  READ ENTITIES OF zc_ai_hitl_req IN LOCAL MODE
    ENTITY HitlReq
    FIELDS ( Status RespondedAt RespondedBy )
    WITH CORRESPONDING #( keys )
    RESULT DATA(result).

  DATA to_update TYPE TABLE FOR UPDATE zc_ai_hitl_req\\HitlReq.

  LOOP AT result ASSIGNING FIELD-SYMBOL(<r>).
    IF <r>-Status = 'RESPONDED'
       AND ( <r>-RespondedAt IS INITIAL OR <r>-RespondedBy IS INITIAL ).

      APPEND VALUE #( %key = <r>-%key
                      RespondedAt = COND #( WHEN <r>-RespondedAt IS INITIAL THEN current_time ELSE <r>-RespondedAt )
                      RespondedBy = COND #( WHEN <r>-RespondedBy IS INITIAL THEN current_user ELSE <r>-RespondedBy )

                      %control-RespondedAt = if_abap_behv=>mk-on
                      %control-RespondedBy = if_abap_behv=>mk-on ) TO to_update.
    ENDIF.
  ENDLOOP.

  IF to_update IS NOT INITIAL.
    MODIFY ENTITIES OF zc_ai_hitl_req IN LOCAL MODE
      ENTITY HitlReq
      UPDATE FIELDS ( RespondedAt RespondedBy )
      WITH to_update
      FAILED DATA(failed_update)
      REPORTED DATA(reported_update).
  ENDIF.

ENDMETHOD.

ENDCLASS.

CLASS lsc_ZC_AI_HITL_REQ DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZC_AI_HITL_REQ IMPLEMENTATION.

  METHOD save_modified.

    " Raise event only on create
    IF create-hitlreq IS NOT INITIAL.
      RAISE ENTITY EVENT zc_ai_hitl_req~HumanInputRequested
        FROM VALUE #(
          FOR req IN create-hitlreq (
            %key = req-%key
            %param-ev_Topic              = req-Topic
            %param-ev_CorrelationId      = req-CorrelationId
            %param-ev_AgentId            = req-AgentId
            %param-ev_NodeId             = req-NodeId
            %param-ev_Reason             = req-Reason
            %param-ev_Prompt             = req-Prompt
            %param-ev_ResponseSchema     = req-ResponseSchema
            %param-ev_PrimaryResultField = req-PrimaryResultField
          )
        ).
    ENDIF.

  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.


ENDCLASS.
