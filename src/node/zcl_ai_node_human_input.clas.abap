CLASS zcl_ai_node_human_input DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.


    METHODS constructor
      IMPORTING
        node_id   TYPE zif_ai_types=>ty_node_id
        agent_id  TYPE zif_ai_types=>ty_agent_id
        topic     TYPE string DEFAULT 'ABAPCHAIN'
        reason    TYPE string DEFAULT 'Human input required'
        prompt    TYPE string DEFAULT 'Please respond'
        schema    TYPE string DEFAULT '{ "type":"object" }'
        primary   TYPE string DEFAULT 'approved'
        requester TYPE REF TO zif_ai_hitl_request_service OPTIONAL.



  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.
    DATA _node_id  TYPE zif_ai_types=>ty_node_id.
    DATA _agent_id TYPE zif_ai_types=>ty_agent_id.

    DATA _topic   TYPE string.
    DATA _reason  TYPE string.
    DATA _prompt  TYPE string.
    DATA _schema  TYPE string.
    DATA _primary TYPE string.

    DATA _requester TYPE REF TO zif_ai_hitl_request_service.



ENDCLASS.


CLASS zcl_ai_node_human_input IMPLEMENTATION.

  METHOD constructor.
    super->constructor( agent_id = agent_id node_id = node_id node_name = 'HITL' ).

    _topic   = topic.
    _reason  = reason.
    _prompt  = prompt.
    _schema  = schema.
    _primary = primary.

    _requester = COND #( WHEN _requester IS BOUND
                           THEN _requester
                           ELSE NEW zcl_ai_hitl_request_service( ) ).
  ENDMETHOD.

  METHOD do_execute.


    " Idempotency: if correlation already exists, don't create a second request
    IF state-hitl_correlation_id IS INITIAL.
      state-hitl_correlation_id = cl_system_uuid=>create_uuid_x16_static( ).
    ENDIF.

    SELECT SINGLE correlation_id
  FROM zai_hitl_req
  WHERE correlation_id = @state-hitl_correlation_id
  INTO @DATA(dummy).

    state-hitl_topic           = _topic.
    state-hitl_reason          = _reason.
    state-hitl_prompt          = _prompt.
    state-hitl_response_schema = _schema.
    state-hitl_primary_field   = _primary.

    _requester->request_input(
      agent_id = _agent_id
      node_id  = _node_id
      state    = state ).

    state-status = zif_ai_types=>gc_workflow_status_waiting.
    state-branch_label = 'WAITING_FOR_HUMAN'.
  ENDMETHOD.

ENDCLASS.
