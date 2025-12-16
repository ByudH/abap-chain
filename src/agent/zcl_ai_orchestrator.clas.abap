CLASS zcl_ai_orchestrator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS run
      IMPORTING
*        it_nodes         TYPE zif_ai_types=>tt_node_registry_lh
*        it_edges         TYPE zif_ai_types=>tt_edge_lh
        node_edge_graph    TYPE zif_ai_types=>th_graph_map
        start_node_id      TYPE zif_ai_types=>ty_node_id
        initial_state      TYPE zif_ai_types=>ts_graph_state OPTIONAL
      RETURNING
        VALUE(final_state) TYPE zif_ai_types=>ts_graph_state.

ENDCLASS.



CLASS zcl_ai_orchestrator IMPLEMENTATION.

  METHOD run.

    DATA(logger) = zcl_abapchain_logger=>get_instance( ).

    DATA state TYPE zif_ai_types=>ts_graph_state.
    state = initial_state.

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
            message   = |Execute node.|
            severity  = if_bali_constants=>c_severity_status ).
        CATCH cx_root.
      ENDTRY.

*      graph_entry-source_node->execute(
*        CHANGING
*            state = state ).

      TRY.
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
            message   = |Node executed. branch_label="{ state-branch_label }" last_tool="{ state-last_tool_name }".|
            severity  = if_bali_constants=>c_severity_information ).
        CATCH cx_root.
      ENDTRY.



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

        logger->log_orchestrator(
              step         = step_count
              current_node = current_node_id
              next_node    = current_node_id
              branch_label = state-branch_label
              message      = |Stop: no outgoing edges.|
              severity     = if_bali_constants=>c_severity_status ).

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

ENDCLASS.
