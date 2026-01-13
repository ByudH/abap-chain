CLASS lhc_HitlReq DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR HitlReq RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR HitlReq RESULT result.

    METHODS Respond FOR MODIFY
      IMPORTING keys FOR ACTION HitlReq~Respond.
    METHODS set_created_audit FOR DETERMINE ON MODIFY
      IMPORTING keys FOR HitlReq~set_created_audit." RESULT result.

ENDCLASS.

CLASS lhc_HitlReq IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD Respond.

    DATA current_time   TYPE timestampl.
    DATA current_user TYPE syuname.

    GET TIME STAMP FIELD current_time.
    current_user = cl_abap_context_info=>get_user_technical_name( ).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<p>).

      DATA(correlation_id)    = <p>-%param-CorrelationId.
      DATA(payload) = <p>-%param-ResponsePayload.

      MODIFY ENTITIES OF zc_ai_hitl_req IN LOCAL MODE
        ENTITY HitlReq
        UPDATE FIELDS ( Status ResponsePayload RespondedAt RespondedBy )
        WITH VALUE #(
          ( CorrelationId   = correlation_id
            Status          = 'RESPONDED'
            ResponsePayload = payload
            RespondedAt     = current_time
            RespondedBy     = current_user )
        ).

      " result = VALUE #( ( %param-CorrelationId = lv_corr ) ).

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

ENDCLASS.

CLASS lsc_ZC_AI_HITL_REQ DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZC_AI_HITL_REQ IMPLEMENTATION.

  METHOD save_modified.

    IF create-hitlreq IS NOT INITIAL.

      " raise the event
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
