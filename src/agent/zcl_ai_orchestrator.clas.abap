CLASS zcl_ai_orchestrator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
        agent_id           TYPE zif_ai_types=>ty_agent_id
        agent_name         TYPE string " expose agent name for storing agent blueprint
        tools              TYPE zif_ai_types=>th_tool_registry_map " expose tool registry for storing agent blueprint
        node_edge_graph    TYPE zif_ai_types=>th_graph_map
        start_node_id      TYPE zif_ai_types=>ty_node_id
        initial_state      TYPE zif_ai_types=>ts_graph_state OPTIONAL
        hitl_wait          TYPE REF TO zif_ai_hitl_wait_strategy OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

    " US2.3: Save the current graph state to the database
    CLASS-METHODS save_checkpoint
      IMPORTING
        agent_id             TYPE zif_ai_types=>ty_agent_id
        node_id              TYPE zif_ai_types=>ty_node_id
        state                TYPE zif_ai_types=>ts_graph_state
      RETURNING
        VALUE(checkpoint_id) TYPE zai_checkpoint-checkpoint_id.

    " US2.4 — Resume execution from a checkpoint
    CLASS-METHODS resume_checkpoint
      IMPORTING
        checkpoint_id TYPE zai_checkpoint-checkpoint_id
      EXPORTING
        agent_id      TYPE zif_ai_types=>ty_agent_id
        node_id       TYPE zif_ai_types=>ty_node_id
        state         TYPE zif_ai_types=>ts_graph_state.

    CLASS-METHODS extract_branch_label
     IMPORTING payload TYPE string primary TYPE string
     RETURNING VALUE(label) TYPE string.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ai_orchestrator IMPLEMENTATION.

  METHOD run.

    DATA(logger) = zcl_abapchain_logger=>get_instance( ).


    DATA state TYPE zif_ai_types=>ts_graph_state.
    state = initial_state.

    DATA(hitl) = hitl_wait.
    IF hitl IS NOT BOUND.
      hitl = NEW zcl_ai_hitl_wait_blocking( ).
      state-hitl_strategy = zif_ai_types=>gc_hitl_strategy_wait.
    ENDIF.



    DATA current_node_id TYPE zif_ai_types=>ty_node_id.
    current_node_id = start_node_id.



    IF current_node_id IS INITIAL.
      " No start node -> nothing to do
      final_state = state.
      RETURN.
    ENDIF.

    DATA graph_entry      TYPE zif_ai_types=>ts_graph_entry.
    DATA edge            TYPE zif_ai_types=>ts_edge.
    DATA edges_for_node  TYPE zif_ai_types=>tt_edge_list.
    DATA next_node_id    TYPE zif_ai_types=>ty_node_id.

    " Optional: avoid infinite loops in bad graphs
    DATA step_count TYPE i VALUE 0.
    CONSTANTS max_steps TYPE i VALUE 50.


    TRY.
        logger->log_orchestrator(
          step         = step_count
          current_node = current_node_id
          next_node    = current_node_id
          branch_label = state-branch_label
          message      = |Orchestrator started. start_node_id={ start_node_id }.|
          severity     = if_bali_constants=>c_severity_status ).
      CATCH cx_root.
    ENDTRY.


    WHILE current_node_id IS NOT INITIAL.

      step_count = step_count + 1.
      IF step_count > max_steps.
        " Safety break: prevent endless loops in broken graphs
        TRY.
            logger->log_orchestrator(
              step         = step_count
              current_node = current_node_id
              next_node    = current_node_id
              branch_label = state-branch_label
              message      = |Stop: max_steps reached ({ max_steps }).|
              severity     = if_bali_constants=>c_severity_error ).
          CATCH cx_root.
        ENDTRY.
        EXIT.
      ENDIF.


      " 1) Find current node in graph map (hashed table)
      CLEAR graph_entry.
      READ TABLE node_edge_graph INTO graph_entry
           WITH KEY source_node_id = current_node_id.


      IF sy-subrc <> 0 OR graph_entry-source_node IS INITIAL.
        " Inconsistent graph: node id not found → stop
        TRY.
            logger->log_orchestrator(
              step         = step_count
              current_node = current_node_id
              next_node    = current_node_id
              branch_label = state-branch_label
              message      = |Stop: node not found or node ref INITIAL.|
              severity     = if_bali_constants=>c_severity_error ).
          CATCH cx_root.
        ENDTRY.
        EXIT.
      ENDIF.


      " 2) Execute node (polymorphic via zif_ai_node)
      TRY.
          logger->log_node(
            node_id   = current_node_id
            node_name = graph_entry-source_node->get_node_name( )
            message   = |Execute node.|
            severity  = if_bali_constants=>c_severity_status ).
        CATCH cx_root.
      ENDTRY.

*      graph_entry-source_node->execute(
*        CHANGING
*            state = state ).
      IF state-skip_current_execute = abap_true.
        state-skip_current_execute = abap_false.
      ELSE.
        TRY.

            " execute node

            graph_entry-source_node->execute(
              CHANGING
                state = state ).


          CATCH cx_root INTO DATA(ex).
            TRY.
                logger->log_error(
                  message = |Node execution failed. node_id={ current_node_id }. { ex->get_text( ) }| ).
              CATCH cx_root.
            ENDTRY.
            " Decide policy: EXIT (soft-stop) or RAISE EXCEPTION ex (fail hard)
            EXIT.
        ENDTRY.


        TRY.
            logger->log_node(
              node_id   = current_node_id
              node_name = graph_entry-source_node->get_node_name( )
              message   = |Node executed. branch_label="{ state-branch_label }" last_tool="{ state-last_tool_name }".|
              severity  = if_bali_constants=>c_severity_information ).
          CATCH cx_root.
        ENDTRY.

      ENDIF.

      IF state-status = zif_ai_types=>gc_workflow_status_waiting
         AND state-hitl_correlation_id IS NOT INITIAL.

        logger->log_orchestrator(
          step         = step_count
          current_node = current_node_id
          next_node    = current_node_id
          branch_label = state-branch_label
          message      = |WAIT_ENTER corr_id={ state-hitl_correlation_id }|
          severity     = if_bali_constants=>c_severity_status ).

        " --- US2.3: PERSISTENCE POINT ---
        " Save state immediately after successful node execution to capture
        " the latest changes (e.g., LLM responses or tool outputs).
        DATA(latest_checkpoint) = zcl_ai_orchestrator=>save_checkpoint(
          agent_id = agent_id
          node_id  = current_node_id
          state    = state
        ).
        state-last_checkpoint_id   = latest_checkpoint.

        hitl->handle_wait(
          EXPORTING
            agent_id = agent_id    " or iv_agent_id
          CHANGING
            state    = state ).

        " Use gc_working_status_paused to indicate if the agent need to be restored in the
        " respond function of zbp_c_ai_hitl_req's local class
        IF state-status = zif_ai_types=>gc_workflow_status_paused.
          " before exiting, save the latest state of checkpoint
          zcl_ai_orchestrator=>save_checkpoint(
          agent_id = agent_id
          node_id  = current_node_id
          state    = state
        ).
          " before exiting, save the structure of the agent
          TRY.
              zcl_ai_agent_repository=>save_agent_blueprint( zcl_ai_agent=>get_agent_blueprint_static(
                EXPORTING
                  agent_id = agent_id
                  agent_name = agent_name
                  node_edge_graph = node_edge_graph
                  start_node_id = start_node_id
                  tools = tools
              ) ).
            CATCH cx_root.
              logger->log_error(
                message = |Agent blueprint save failed. agent_id={ agent_id }| ).
          ENDTRY.
          EXIT.
        ENDIF.

        logger->log_orchestrator(
          step         = step_count
          current_node = current_node_id
          next_node    = current_node_id
          branch_label = state-branch_label
          message      = |WAIT_RESUME status="{ state-status }"|
          severity     = if_bali_constants=>c_severity_information ).

      ENDIF.



      " 3) Collect outgoing edges for this node
      CLEAR edges_for_node.
      edges_for_node = graph_entry-next_nodes.

      IF edges_for_node IS INITIAL.
        " No outgoing edges → implicit END
        TRY.
            logger->log_orchestrator(
              step         = step_count
              current_node = current_node_id
              next_node    = current_node_id
              branch_label = state-branch_label
              message      = |Stop: no outgoing edges.|
              severity     = if_bali_constants=>c_severity_status ).
          CATCH cx_root.
        ENDTRY.
        EXIT.
      ENDIF.


      " Sort edges by priority (lower = higher priority)
      SORT edges_for_node BY priority ASCENDING target_node_id ASCENDING.

      " 4) Determine next node:
      "    a) ON_CONTROL: condition_value = state.branch_label
      "    b) ALWAYS: unconditional fallback
      CLEAR next_node_id.

      " First: ON_CONTROL
      " TODO: normalize state-branch_label (to_upper either in node or here)
      LOOP AT edges_for_node INTO edge
           WHERE condition       = zif_ai_types=>gc_cond_on_control
             AND condition_value = state-branch_label.
        next_node_id = edge-target_node_id.
        EXIT.
      ENDLOOP.

      " If no ON_CONTROL matched, try ALWAYS
      IF next_node_id IS INITIAL.
        LOOP AT edges_for_node INTO edge
             WHERE condition = zif_ai_types=>gc_cond_always.
          next_node_id = edge-target_node_id.
          EXIT.
        ENDLOOP.
      ENDIF.

      " No matching edge -> terminate
      IF next_node_id IS INITIAL.

        TRY.
            logger->log_orchestrator(
              step         = step_count
              current_node = current_node_id
              next_node    = current_node_id
              branch_label = state-branch_label
              message      = |Stop: no outgoing edges.|
              severity     = if_bali_constants=>c_severity_status ).
          CATCH cx_bali_runtime.
        ENDTRY.

        EXIT.
      ENDIF.


      TRY.
          logger->log_orchestrator(
            step         = step_count
            current_node = current_node_id
            next_node    = next_node_id
            branch_label = state-branch_label
            message      = |Route decided. condition resolution done.|
            severity     = if_bali_constants=>c_severity_information ).
        CATCH cx_root.
      ENDTRY.

      " Move to next node
      current_node_id = next_node_id.

    ENDWHILE.

    final_state = state.

  ENDMETHOD.

  METHOD save_checkpoint.
    " US2.3: Implementation to persist state
    DATA: ls_checkpoint TYPE zai_checkpoint.

    " 1. Serialize the current state structure into a JSON string
    " We use the standard JSON library to convert the state into a string format
    DATA(lv_state_json) = /ui2/cl_json=>serialize( data = state ).

    " 2. Prepare the database record
    TRY.
        ls_checkpoint-client        = sy-mandt.
        " Generate a unique ID for this specific snapshot
        ls_checkpoint-checkpoint_id = cl_system_uuid=>create_uuid_x16_static( ).

        " --- Advanced Optimization: Mapping Agent ID ---
        ls_checkpoint-agent_id      = agent_id.

        " Note: In your current static 'run' method, you might need a way to pass agent_id.
        " For now, we focus on node and state.
        ls_checkpoint-node_id       = node_id.
        GET TIME STAMP FIELD ls_checkpoint-timestamp.
        ls_checkpoint-state_data    = lv_state_json.

        " 3. Insert into the database table you created in Step 1
        INSERT zai_checkpoint FROM @ls_checkpoint.

        IF sy-subrc <> 0.
          " Optional: Log failure if database insertion fails
        ENDIF.

        checkpoint_id = ls_checkpoint-checkpoint_id.

      CATCH cx_uuid_error.
        " Handle UUID generation exception if necessary
    ENDTRY.
  ENDMETHOD.

  METHOD resume_checkpoint.

    DATA: ls_checkpoint TYPE zai_checkpoint,
          lv_state_json TYPE string.

    " 1) Load checkpoint
    SELECT SINGLE *
      FROM zai_checkpoint
      WHERE checkpoint_id = @checkpoint_id
      INTO @ls_checkpoint.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_no_handler.
    ENDIF.

    " 2) Deserialize state
    lv_state_json = ls_checkpoint-state_data.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_state_json
      CHANGING
        data = state ).

    " 3) Restore execution pointers
    node_id  = ls_checkpoint-node_id.
    agent_id = ls_checkpoint-agent_id.

  ENDMETHOD.

  METHOD extract_branch_label.
    " TODO: replace with real JSON parsing
    DATA primary_value TYPE string.
    primary_value = primary.

    TRANSLATE primary_value TO LOWER CASE.

    IF primary_value = 'approved'.
      IF payload CS '"approved":true'
         OR payload CS '"approved" : true'
         OR payload CS '"approved": true'.
        label = 'APPROVED'.
      ELSE.
        label = 'REJECTED'.
      ENDIF.
    ELSE.
      label = 'HITL_DONE'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
