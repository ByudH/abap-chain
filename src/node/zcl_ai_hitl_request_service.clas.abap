CLASS zcl_ai_hitl_request_service DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ai_hitl_request_service.
ENDCLASS.

CLASS zcl_ai_hitl_request_service IMPLEMENTATION.

  METHOD zif_ai_hitl_request_service~request_input.

    ASSERT state-hitl_correlation_id IS NOT INITIAL.
    ASSERT agent_id IS NOT INITIAL.
    ASSERT node_id IS NOT INITIAL.
    " Create request via EML so that saver raises event on COMMIT ENTITIES
    MODIFY ENTITIES OF zc_ai_hitl_req
      ENTITY HitlReq
      CREATE FIELDS (
        CorrelationId
        AgentId
        NodeId
        Topic
        Status
        Reason
        Prompt
        ResponseSchema
        PrimaryResultField
      )
      WITH VALUE #(
        ( %cid               = 'HITL1'
          CorrelationId      = state-hitl_correlation_id
          AgentId            = agent_id
          NodeId             = node_id
          Topic              = state-hitl_topic
          Status             = 'NEW'
          Reason             = state-hitl_reason
          Prompt             = state-hitl_prompt
          ResponseSchema     = state-hitl_response_schema
          PrimaryResultField = state-hitl_primary_field )

      )
      FAILED DATA(failed)
      REPORTED DATA(reported).

    IF failed IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_sy_no_handler.
    ENDIF.

    COMMIT ENTITIES.

  ENDMETHOD.

ENDCLASS.
