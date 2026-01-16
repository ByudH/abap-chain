
CLASS zcl_ai_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE " enforce factory usage
  GLOBAL FRIENDS zcl_ai_agent_builder. " allow builder access to the agent_id

  PUBLIC SECTION.

    DATA agent_name      TYPE string                    READ-ONLY.
    DATA agent_id        TYPE zif_ai_types=>ty_agent_id READ-ONLY.

    " Runtime graph representation
    DATA node_edge_graph TYPE zif_ai_types=>th_graph_map      READ-ONLY.
    DATA start_node_id   TYPE zif_ai_types=>ty_node_id        READ-ONLY.

    " Agent-level tool registry
    DATA tools           TYPE zif_ai_types=>th_tool_registry_map READ-ONLY.

    " Factory method – called by the builder
    CLASS-METHODS create
      IMPORTING
        agent_name      TYPE string
        node_edge_graph TYPE zif_ai_types=>th_graph_map
        start_node_id   TYPE zif_ai_types=>ty_node_id
        tools           TYPE zif_ai_types=>th_tool_registry_map OPTIONAL
      RETURNING
        VALUE(agent)    TYPE REF TO zcl_ai_agent.

    METHODS run
      IMPORTING
        initial_state      TYPE zif_ai_types=>ts_graph_state OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

    " Methods for serialization
    METHODS get_agent_blueprint
      RETURNING
        VALUE(agent_blueprint) TYPE zif_ai_types=>ts_agent_blueprint.
  PRIVATE SECTION.

    METHODS constructor
      IMPORTING
        agent_name      TYPE string
        node_edge_graph TYPE zif_ai_types=>th_graph_map
        start_node_id   TYPE zif_ai_types=>ty_node_id
        tools           TYPE zif_ai_types=>th_tool_registry_map.

    METHODS resume_from_checkpoint
      IMPORTING
        checkpoint_id      TYPE zai_checkpoint-checkpoint_id
        hitl_primary       TYPE string OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

ENDCLASS.

CLASS zcl_ai_agent IMPLEMENTATION.

  METHOD run.
    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    TRY.
        logger->start_run(
          agent_name = me->agent_name
          agent_id   = me->agent_id ).
      CATCH cx_root INTO DATA(err).
        logger->log_error( err->get_text( ) ).
        " Logging must never break execution
    ENDTRY.

    final_state = zcl_ai_orchestrator=>run(
      agent_id        = me->agent_id
      node_edge_graph = me->node_edge_graph
      start_node_id   = me->start_node_id
      initial_state   = initial_state ).

    TRY.
        logger->save_and_get_handle( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    " Basic metadata
    me->agent_name = agent_name.

    " Create a new agent_id
    me->agent_id = zcl_ai_utils=>generate_uuid( ).

    " Graph structure
    me->node_edge_graph = node_edge_graph.
    me->start_node_id   = start_node_id.

    " Tools
    me->tools = tools.
  ENDMETHOD.


  METHOD resume_from_checkpoint.

  DATA agent_id TYPE zif_ai_types=>ty_agent_id.
  DATA node_id  TYPE zif_ai_types=>ty_node_id.
  DATA state   TYPE zif_ai_types=>ts_graph_state.

  zcl_ai_orchestrator=>resume_checkpoint(
    EXPORTING
      checkpoint_id = checkpoint_id
    IMPORTING
      agent_id      = agent_id
      node_id       = node_id
      state         = state ).

  " Ensure we have corr id to fetch the HITL answer
  IF state-hitl_correlation_id IS INITIAL.
    state-status = zif_ai_types=>gc_workflow_status_error.
    state-branch_label = 'NO_CORR_ID'.
    final_state = state.
    RETURN.
  ENDIF.

  " Load HITL response
  SELECT SINGLE status, response_payload, primary_result_field
    FROM zai_hitl_req
    WHERE correlation_id = @state-hitl_correlation_id
    INTO (@DATA(db_status), @DATA(db_payload), @DATA(db_primary)).

  IF sy-subrc <> 0 OR db_status <> 'RESPONDED'.
    state-status = zif_ai_types=>gc_workflow_status_waiting.
    final_state = state.
    RETURN.
  ENDIF.

  state-hitl_response_payload = db_payload.
  state-status                = zif_ai_types=>gc_workflow_status_running.

  IF db_primary IS INITIAL.
    db_primary = state-hitl_primary_field.
  ENDIF.

  state-branch_label = zcl_ai_orchestrator=>extract_branch_label(
    payload = db_payload
    primary = CONV string( db_primary ) ).

  " IMPORTANT: do not re-execute the HITL node; continue routing from it
  state-skip_current_execute = abap_true.

  final_state = zcl_ai_orchestrator=>run(
    agent_id        = me->agent_id        " must match
    node_edge_graph = me->node_edge_graph
    start_node_id   = node_id             " resume at the checkpoint node
    initial_state   = state
    hitl_wait       = NEW zcl_ai_hitl_wait_pause_check( ) ). " or a no-op

ENDMETHOD.

  METHOD create.
    CREATE OBJECT agent
      EXPORTING
        agent_name      = agent_name
        node_edge_graph = node_edge_graph
        start_node_id   = start_node_id
        tools           = tools.
  ENDMETHOD.

  METHOD get_agent_blueprint.
    agent_blueprint-agent_id = me->agent_id.
    agent_blueprint-start_node_id = me->start_node_id.
    agent_blueprint-agent_name = me->agent_name.
    LOOP AT node_edge_graph INTO DATA(graph_entry).
      DATA node_blueprint TYPE zif_ai_types=>ts_node_blueprint.
      DATA edge_blueprints TYPE zif_ai_types=>tt_edge_blueprints.

      CLEAR node_blueprint.
      CLEAR edge_blueprints.

      node_blueprint-node_id = graph_entry-source_node_id.
      node_blueprint-class_name = graph_entry-source_node->get_node_type( ).
      node_blueprint-config = graph_entry-source_node->get_configuration( ).

      LOOP AT graph_entry-next_nodes INTO DATA(edge_entry).
        DATA edge_blueprint TYPE zif_ai_types=>ts_edge_blueprint.

        CLEAR edge_blueprint.

        edge_blueprint-target_node_id = edge_entry-target_node_id.
        edge_blueprint-condition = edge_entry-condition.
        edge_blueprint-condition_value = edge_entry-condition_value.
        edge_blueprint-priority = edge_entry-priority.
        APPEND edge_blueprint TO edge_blueprints.
      ENDLOOP.

      node_blueprint-next_nodes = edge_blueprints.
      APPEND node_blueprint TO agent_blueprint-graph_blueprint.


    ENDLOOP.

    LOOP AT tools INTO DATA(tool_entry).
      DATA tool_blueprint TYPE zif_ai_types=>ts_tool_blueprint.
      CLEAR tool_blueprint.
      tool_blueprint-tool_name = tool_entry-tool_name.
      tool_blueprint-tool_class = tool_entry-tool_endpoint->get_tool_type( ).
      tool_blueprint-tool_description = tool_entry-tool_description.
      APPEND tool_blueprint TO agent_blueprint-tool_registry_blueprint.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
