CLASS zcl_ai_node_human_input DEFINITION
  PUBLIC
  INHERITING FROM zcl_ai_node_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.


    METHODS constructor
      IMPORTING
        " if changing mandatory input parameters, it will break logic in build_from_blueprint of builder
        name      TYPE string DEFAULT 'HITL'
        topic     TYPE string DEFAULT 'ABAPCHAIN'
        reason    TYPE string DEFAULT 'Human input required'
        prompt    TYPE string DEFAULT 'Please respond'
        schema    TYPE string DEFAULT '{ "type":"object" }'
        primary   TYPE string DEFAULT 'approved'
        requester TYPE REF TO zif_ai_hitl_request_service OPTIONAL.

    METHODS zif_ai_node~get_configuration REDEFINITION.
    METHODS zif_ai_node~set_configuration REDEFINITION.


  PROTECTED SECTION.
    METHODS do_execute REDEFINITION.

  PRIVATE SECTION.

    DATA _topic   TYPE string.
    DATA _reason  TYPE string.
    DATA _prompt  TYPE string.
    DATA _schema  TYPE string.
    DATA _primary TYPE string.

    DATA _requester TYPE REF TO zif_ai_hitl_request_service.

    TYPES: BEGIN OF ts_hitl_node_config,
             name      TYPE string,
             topic     TYPE string,
             reason    TYPE string,
             prompt    TYPE string,
             schema    TYPE string,
             primary   TYPE string,
*             requester TYPE string,
           END OF ts_hitl_node_config.



ENDCLASS.


CLASS zcl_ai_node_human_input IMPLEMENTATION.

  METHOD constructor.
    super->constructor( node_name = name ).

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

  METHOD zif_ai_node~get_configuration.
    data(config) = value ts_hitl_node_config(
      name      = me->node_name
      topic     = _topic
      reason    = _reason
      prompt    = _prompt
      schema    = _schema
      primary   = _primary
    ).

    configuration = xco_cp_json=>data->from_abap( config )->to_string( ).
  ENDMETHOD.
  METHOD zif_ai_node~set_configuration.
    data(config) = value ts_hitl_node_config( ).

    xco_cp_json=>data->from_string( configuration )->write_to( REF #( config ) ).
    me->node_name = config-name.
    _topic   = config-topic.
    _reason  = config-reason.
    _prompt  = config-prompt.
    _schema  = config-schema.
    _primary = config-primary.
    _requester = NEW zcl_ai_hitl_request_service( ).

  ENDMETHOD.

ENDCLASS.
