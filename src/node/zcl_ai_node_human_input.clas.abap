CLASS zcl_ai_node_human_input DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.


    METHODS constructor
      IMPORTING
        topic     TYPE string DEFAULT 'ABAPCHAIN'
        reason    TYPE string DEFAULT 'Human input required'
        prompt    TYPE string DEFAULT 'Please respond'
        schema    TYPE string DEFAULT '{ "type":"object" }'
        primary   TYPE string DEFAULT 'approved'
        requester TYPE REF TO zif_ai_hitl_request_service OPTIONAL.



  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.

    DATA _topic   TYPE string.
    DATA _reason  TYPE string.
    DATA _prompt  TYPE string.
    DATA _schema  TYPE string.
    DATA _primary TYPE string.

    DATA _requester TYPE REF TO zif_ai_hitl_request_service.



ENDCLASS.


CLASS zcl_ai_node_human_input IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_name = 'HITL' ).

    _topic   = topic.
    _reason  = reason.
    _prompt  = prompt.
    _schema  = schema.
    _primary = primary.

    _requester = COND #( WHEN requester IS BOUND
                       THEN requester
                       ELSE NEW zcl_ai_hitl_request_service( ) ).
  ENDMETHOD.

  METHOD do_execute.

    IF state-hitl_correlation_id IS INITIAL.
      state-hitl_correlation_id = cl_system_uuid=>create_uuid_x16_static( ).
    ENDIF.

    " Fill state
    state-hitl_topic           = _topic.
    state-hitl_reason          = _reason.
    state-hitl_prompt          = _prompt.
    state-hitl_response_schema = _schema.
    state-hitl_primary_field   = _primary.

    " Create request if not exists
    SELECT SINGLE correlation_id
      FROM zai_hitl_req
      WHERE correlation_id = @state-hitl_correlation_id
      INTO @DATA(dummy).

    IF sy-subrc <> 0.
      _requester->request_input(
        agent_id = me->agent_id
        node_id  = me->node_id
        state    = state ).
    ENDIF.

    state-status       = zif_ai_types=>gc_workflow_status_waiting.
    state-branch_label = zif_ai_types=>gc_workflow_status_waiting.

  ENDMETHOD.

ENDCLASS.
